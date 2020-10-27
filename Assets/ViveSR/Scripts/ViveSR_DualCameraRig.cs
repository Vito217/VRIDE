//========= Copyright 2017, HTC Corporation. All rights reserved. ===========

using UnityEngine;
using System.Collections.Generic;

namespace Vive.Plugin.SR
{
    [ExecuteInEditMode]
    public class ViveSR_DualCameraRig : ViveSR_Module
    {
        public Camera OriginalCamera;
        public Camera DualCameraLeft;
        public Camera DualCameraRight;
        public Camera RenderCameraLeft;
        public Camera RenderCameraRight;
        public ViveSR_DualCameraImageRenderer DualCameraImageRenderer;
        public ViveSR_TrackedCamera TrackedCameraLeft;
        public ViveSR_TrackedCamera TrackedCameraRight;
        public GameObject RenderCameraFrameLeft;
        public GameObject RenderCameraFrameRight;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable VirtualCamera has changed to DualCameraLeft.
        * @warning The variable will remove in the future.
        */
        public Camera VirtualCamera
        {
            get { return DualCameraLeft; }
            set { DualCameraLeft = value; }
        }
        #endregion
        /// <summary>
        /// Set whether to render virtual objects or pass through images.
        /// There are three modes: VIRTUAL, REAL, and MIX.
        /// VIRTUAL:
        ///   If main camera exists, disable the dual cameras and the render cameras, and the enable the main camera.
        ///   If there is no main camera, enable the dual cameras and the render cameras, and render only virtual objects.
        /// REAL: Render only pass through images.
        /// MIX: Render both virtual objects and pass through images.
        /// </summary>
        public DualCameraDisplayMode Mode = DualCameraDisplayMode.MIX;
        private DualCameraDisplayMode PreviousMode = DualCameraDisplayMode.MIX;
        /// <summary>
        /// Besides the built-in virtual camera, users can drag other cameras here
        /// to render virtual objects. Rach camera will be split into a left and a right camera,
        /// and renders to the render textures.
        /// </summary>
        [Tooltip("Drag cameras here if they also render virtual objects. Please avoid duplicate objects.")]
        public Camera[] RegisteredVirtualCameras;

        public static DualCameraStatus DualCameraStatus { get; private set; }
        public static string LastError { get; private set; }

        private struct RenderTexturePair
        {
            public RenderTexture Left;
            public RenderTexture Right;
        }

        /// <summary>
        /// Render textures are prepared at different scaling level.
        /// The constant is the count of the levels.
        /// </summary>
        public const int RenderTextureLevelsCount = 2;

        /// <summary>
        /// Render textures are prepared at different scaling level.
        /// The array stores the scale factor of each level.
        /// </summary>
        private float[] RenderTextureScaleFactors = new float[RenderTextureLevelsCount] {1, 0.5f};

        /// <summary>
        /// Render textures are prepared at different scaling level.
        /// </summary>
        private RenderTexturePair[] RenderTextureLevels = new RenderTexturePair[RenderTextureLevelsCount];

        /// <summary>
        /// The current level of render textures being used.
        /// </summary>
        private int RenderTextureCurrentLevel = 0;

        /// <summary>
        /// Current ratio of view camera frame which will reduce user FOV for not seeing black border in moving.
        /// </summary>
        private float WidthViewCameraFrameRatio = 1.0f;
        /// <summary>
        /// Current ratio of view camera frame which will reduce user FOV for not seeing black border in moving.
        /// </summary>
        private float HeightViewCameraFrameRatio = 1.0f;

        /// <summary>
        /// Manage the registered virtual cameras.
        /// </summary>
        private List<ViveSR_VirtualCameraRig> RegisteredVirtualCameraRigs = new List<ViveSR_VirtualCameraRig>();

        private ViveSR_DualCameraRig() { }
        private static ViveSR_DualCameraRig Mgr = null;
        public static ViveSR_DualCameraRig Instance
        {
            get
            {
                if (Mgr == null)
                {
                    Mgr = FindObjectOfType<ViveSR_DualCameraRig>();
                }
                //if (Mgr == null)
                //{
                //    Debug.LogError("ViveSR_DualCameraManager does not be attached on GameObject");
                //}
                return Mgr;
            }
        }

        private void Awake()
        {
#if UNITY_EDITOR
            //error in opening project
            if (Application.isEditor) ViveSR_Settings.Update();
#endif

            // Enable a global shader keyword for linear color space,
            // such that pass through images are rendered with correct color.
            if (QualitySettings.activeColorSpace == ColorSpace.Linear)
            {
                Shader.EnableKeyword("SRWORKS_GAMMA_TO_LINEAR");
            }
            else
            {
                Shader.DisableKeyword("SRWORKS_GAMMA_TO_LINEAR");
            }

            // If the original camera is null, reference it to the main camera.
            // The resulting original camera can still be null if there is no main camera.
            if (OriginalCamera == null) OriginalCamera = Camera.main;
        }

        private void Update()
        {
            // Update the dual camera display mode.
            if (Mode != PreviousMode)
            {
                SetMode(Mode);
                PreviousMode = Mode;
            }
        }

        private void OnDestroy()
        {
            // Release the GPU resources of the render textures.
            foreach (var renderTexturePair in RenderTextureLevels)
            {
                if (renderTexturePair.Left) renderTexturePair.Left.Release();
                if (renderTexturePair.Right) renderTexturePair.Right.Release();
            }
        }

        public override bool Initial()
        {
            DualCameraStatus = DualCameraStatus.IDLE;
            if (!ViveSR.Instance.InitializePassThroughModule)
            {
                return false;
            }
            if (ViveSR.FrameworkStatus == FrameworkStatus.WORKING)
            {
                int result = ViveSR_DualCameraImageCapture.Initial();
                if (result != (int)Error.WORK)
                {
                    DualCameraStatus = DualCameraStatus.ERROR;
                    LastError = "[ViveSR] Initial Camera error " + result;
                    Debug.LogError(LastError);
                    return false;
                }

                // Create render textures of different scaling levels.
                // Set the level 0 render texture size to the pass through image size if possible.
                CreateRenderTextures(ViveSR_DualCameraImageCapture.UndistortedImageWidth, ViveSR_DualCameraImageCapture.UndistortedImageHeight);

                // Use the level 0 render textures by default.
                RenderTextureCurrentLevel = 0;
                var renderTextureLeft = RenderTextureLevels[0].Left;
                var renderTextureRight = RenderTextureLevels[0].Right;

                // Make the dual cameras render to the render textures.
                // Set the camera target textures before setting the camera field of view and pose.
                DualCameraLeft.targetTexture = renderTextureLeft;
                DualCameraRight.targetTexture = renderTextureRight;

                // After setting the target texture to a render texture, the camera becomes a non-VR camera.
                // Set the camera vertical field of view.
                DualCameraLeft.fieldOfView = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageHeight, (float) ViveSR_DualCameraImageCapture.FocalLengthLeft) * Mathf.Rad2Deg;
                DualCameraRight.fieldOfView = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageHeight, (float) ViveSR_DualCameraImageCapture.FocalLengthRight) * Mathf.Rad2Deg;
                // Set the camera pose to identity.
                DualCameraLeft.transform.localPosition = Vector3.zero;
                DualCameraLeft.transform.localRotation = Quaternion.identity;
                DualCameraRight.transform.localPosition = Vector3.zero;
                DualCameraRight.transform.localRotation = Quaternion.identity;

                // Make the render planes use the render textures.
                var renderPlaneLeft = TrackedCameraLeft.RenderPlane;
                var renderPlaneRight = TrackedCameraRight.RenderPlane;
                renderPlaneLeft.SetMainTexture(renderTextureLeft);
                renderPlaneRight.SetMainTexture(renderTextureRight);

                // Set the render plane shader.
                var unlitTextureShader = Shader.Find("Unlit/Texture");
                renderPlaneLeft.SetShader(unlitTextureShader);
                renderPlaneRight.SetShader(unlitTextureShader);

                // Create a virtual camera rig for each registered virtual camera.
                foreach (var registeredVirtualCamera in RegisteredVirtualCameras)
                {
                    // Create a virtual camera rig.
                    var registeredVirtualCameraRig = CreateVirtualCameraRig(registeredVirtualCamera,
                        renderTextureLeft, renderTextureRight, DualCameraLeft.fieldOfView, DualCameraRight.fieldOfView);
                    // Store the virtual camera rig.
                    RegisteredVirtualCameraRigs.Add(registeredVirtualCameraRig);
                }

                if (TrackedCameraLeft != null)
                {
                    if (TrackedCameraLeft.ImagePlane != null) TrackedCameraLeft.ImagePlane.Initial(
                        ViveSR_DualCameraImageCapture.UndistortedImageWidth,
                        ViveSR_DualCameraImageCapture.UndistortedImageHeight,
                        ViveSR_DualCameraImageCapture.UndistortedCxLeft,
                        ViveSR_DualCameraImageCapture.UndistortedCyLeft,
                        ViveSR_DualCameraImageCapture.FocalLengthLeft,
                        true);
                    if (TrackedCameraLeft.RenderPlane != null) TrackedCameraLeft.RenderPlane.Initial(
                        renderTextureLeft.width,
                        renderTextureLeft.height,
                        0.5f * renderTextureLeft.width,
                        0.5f * renderTextureLeft.height,
                        GetFocalLength(DualCameraLeft),
                        false);
                }
                if (TrackedCameraRight != null)
                {
                    if (TrackedCameraRight.ImagePlane != null) TrackedCameraRight.ImagePlane.Initial(
                        ViveSR_DualCameraImageCapture.UndistortedImageWidth,
                        ViveSR_DualCameraImageCapture.UndistortedImageHeight,
                        ViveSR_DualCameraImageCapture.UndistortedCxRight,
                        ViveSR_DualCameraImageCapture.UndistortedCyRight,
                        ViveSR_DualCameraImageCapture.FocalLengthRight,
                        true);
                    if (TrackedCameraRight.RenderPlane != null) TrackedCameraRight.RenderPlane.Initial(
                        renderTextureRight.width,
                        renderTextureRight.height,
                        0.5f * renderTextureRight.width,
                        0.5f * renderTextureRight.height,
                        GetFocalLength(DualCameraRight),
                        false);
                }

                SetViewCameraFrame(WidthViewCameraFrameRatio, HeightViewCameraFrameRatio);

                DualCameraStatus = DualCameraStatus.WORKING;
                SetMode(Mode);
                return true;
            }       
            return false;
        }

        public override bool Release()
        {
            DualCameraStatus = DualCameraStatus.IDLE;
            if (!ViveSR.Instance.InitializePassThroughModule)
            {
                return false;
            }
            ViveSR_DualCameraImageCapture.EnableDepthProcess(false);
            ViveSR_DualCameraImageCapture.Release();
            return true;
        }

        /// <summary>
        /// Set whether to render virtual objects or pass through images.
        /// There are three modes: VIRTUAL, REAL, and MIX.
        /// VIRTUAL:
        ///   If main camera exists, disable the dual cameras and the render cameras, and the enable the main camera.
        ///   If there is no main camera, enable the dual cameras and the render cameras, and render only virtual objects.
        /// REAL: Render only pass through images.
        /// MIX: Render both virtual objects and pass through images.
        /// </summary>
        /// <param name="mode">VIRTUAL, REAL or MIX</param>
        public void SetMode(DualCameraDisplayMode mode)
        {
            var enableSRWorksCameras = true;
            if ((mode == DualCameraDisplayMode.VIRTUAL) && (OriginalCamera != null))
            {
                enableSRWorksCameras = false;
            }
            EnableCameras(enableSRWorksCameras, mode);
            Mode = mode;
        }

        /// <summary>
        /// Enable/disable SRWorks cameras and set their culling masks.
        /// Also enable/disable the non-SRWorks camera (the OriginalCamera) if SRWorks cameras are disabled/enabled.
        /// </summary>
        private void EnableCameras(bool enableSRWorksCameras, DualCameraDisplayMode mode)
        {
            // Activate or deactivate non-SRWorks cameras.
            // If the original camera is null, reference it to the main camera.
            // The resulting original camera can still be null if there is no main camera.
            if (OriginalCamera == null) OriginalCamera = Camera.main;
            if (OriginalCamera != null) OriginalCamera.enabled = !enableSRWorksCameras;

            // Activate or deactivate SRWorks cameras.
            DualCameraLeft.enabled = enableSRWorksCameras;
            DualCameraRight.enabled = enableSRWorksCameras;
            RenderCameraLeft.enabled = enableSRWorksCameras;
            RenderCameraRight.enabled = enableSRWorksCameras;

            TrackedCameraLeft.gameObject.SetActive(enableSRWorksCameras);
            TrackedCameraRight.gameObject.SetActive(enableSRWorksCameras);

            if (!enableSRWorksCameras) return;

            // Set the dual camera culling mask.
            var builtinLayersMask = LayerMask.GetMask(ViveSR_Layers.DefaultLayerName, ViveSR_Layers.TransparentFXLayerName,
                ViveSR_Layers.IgnoreRaycastLayerName, ViveSR_Layers.WaterLayerName, ViveSR_Layers.UILayerName);
            var dualCameraLeftLayerMask = LayerMask.GetMask(ViveSR_Layers.DualCameraLeftLayerName);
            var dualCameraRightLayerMask = LayerMask.GetMask(ViveSR_Layers.DualCameraRightLayerName);
            switch (mode)
            {
                case DualCameraDisplayMode.VIRTUAL:
                    // Add the built-in layers.
                    DualCameraLeft.cullingMask |= builtinLayersMask;
                    DualCameraRight.cullingMask |= builtinLayersMask;
                    // Remove the dual camera layers.
                    DualCameraLeft.cullingMask &= ~dualCameraLeftLayerMask;
                    DualCameraRight.cullingMask &= ~dualCameraRightLayerMask;
                    break;
                case DualCameraDisplayMode.REAL:
                    // Remove the built-in layers.
                    DualCameraLeft.cullingMask &= ~builtinLayersMask;
                    DualCameraRight.cullingMask &= ~builtinLayersMask;
                    // Add the dual camera layers.
                    DualCameraLeft.cullingMask |= dualCameraLeftLayerMask;
                    DualCameraRight.cullingMask |= dualCameraRightLayerMask;
                    break;
                case DualCameraDisplayMode.MIX:
                    // Add the built-in layers.
                    DualCameraLeft.cullingMask |= builtinLayersMask;
                    DualCameraRight.cullingMask |= builtinLayersMask;
                    // Add the dual camera layers.
                    DualCameraLeft.cullingMask |= dualCameraLeftLayerMask;
                    DualCameraRight.cullingMask |= dualCameraRightLayerMask;
                    break;
            }
        }

        /// <summary>
        /// Set the left camera pose and the right camera pose.
        /// The poses are used to update the image plane poses and the virtual camera poses.
        /// </summary>
        public void SetCameraPoses(Vector3 positionLeft, Quaternion rotationLeft, Vector3 positionRight, Quaternion rotationRight)
        {
            // Set the image plane poses.
            TrackedCameraLeft.transform.localPosition = positionLeft;
            TrackedCameraLeft.transform.localRotation = rotationLeft;
            TrackedCameraRight.transform.localPosition = positionRight;
            TrackedCameraRight.transform.localRotation = rotationRight;

            // Set the virtual camera poses.
            foreach (var registeredVirtualCameraRig in RegisteredVirtualCameraRigs)
            {
                registeredVirtualCameraRig.SetCameraPoses(positionLeft, rotationLeft, positionRight, rotationRight);
            }
        }

        /// <summary>
        /// Instead of calling GameObject.SetActive() on a registered virtual camera,
        /// use this method to activate/deactivate a registered virtual camera.
        /// The reason is that when a virtual camera is registered, additional game objects are created,
        /// so let the ViveSR_DualCameraRig active/deactivate the virtual camera as well as the
        /// additional game objects.
        /// </summary>
        public void ActivateRegisteredVirtualCamera(Camera camera, bool active)
        {
            // Find the virtual camera rig whose left camera is the input camera.
            var registeredVirtualCameraRig = RegisteredVirtualCameraRigs.Find(x => x.CameraLeft == camera);
            if (registeredVirtualCameraRig == null)
            {
                return;
            }
            // Activate/deactivate the game object (as well as the left/right camera game object).
            registeredVirtualCameraRig.SetGameObjectsActive(active);
        }

        /// <summary>
        /// Attach a transform under the TrackedCamera (Left)'s transfrom.
        /// </sumamry>
        public void AttachToTrackedCameraLeft(Transform transform)
        {
            transform.parent = TrackedCameraLeft.transform;
        }

        /// <summary>
        /// Set which scaling level of render textures to use.
        /// Render texture size is smaller at higher scaling level.
        /// <param name="level">A number in range [0, RenderTextureLevelsCount).</param>
        /// </summary>
        public void SetRenderTextureLevel(int level)
        {
            if (level < 0 || level >= RenderTextureLevelsCount || level == RenderTextureCurrentLevel) return;

            // Release the GPU resources of the unused render textures.
            // The GPU resources will be automatically created again by Unity when they are used.
            RenderTextureLevels[RenderTextureCurrentLevel].Left.Release();
            RenderTextureLevels[RenderTextureCurrentLevel].Right.Release();

            // Update the current level.
            RenderTextureCurrentLevel = level;

            // Set camera target textures and render plane textures.
            var renderTextureLeft = RenderTextureLevels[level].Left;
            var renderTextureRight = RenderTextureLevels[level].Right;
            DualCameraLeft.targetTexture = renderTextureLeft;
            DualCameraRight.targetTexture = renderTextureRight;
            foreach (var registeredVirtualCameraRig in RegisteredVirtualCameraRigs)
            {
                registeredVirtualCameraRig.SetTargetTextures(renderTextureLeft, renderTextureRight);
            }
            TrackedCameraLeft.RenderPlane.SetMainTexture(renderTextureLeft);
            TrackedCameraRight.RenderPlane.SetMainTexture(renderTextureRight);
        }

        /// <summary> Current ratio of view camera frame which will reduce user FOV for not seeing black border in moving. </summary>
        public void SetViewCameraFrame(float widthRatio,float heightRatio)
        {
            WidthViewCameraFrameRatio = widthRatio;
            HeightViewCameraFrameRatio = heightRatio;
            if (RenderCameraLeft != null)
            {
                ViveSR_ViewCameraFrame ViewCameraFrame = RenderCameraLeft.GetComponentInChildren<ViveSR_ViewCameraFrame>();
                ViewCameraFrame.SetFrame(WidthViewCameraFrameRatio, HeightViewCameraFrameRatio);

            }
            if (RenderCameraRight != null)
            {
                ViveSR_ViewCameraFrame ViewCameraFrame = RenderCameraRight.GetComponentInChildren<ViveSR_ViewCameraFrame>();
                ViewCameraFrame.SetFrame(WidthViewCameraFrameRatio, HeightViewCameraFrameRatio);
            }
        }

        /// <summary>
        /// Get the focal length of a camera.
        /// </summary>
        /// <param name="camera">A camera</param>
        public static float GetFocalLength(Camera camera)
        {
            var halfVerticalFOV = 0.5f * camera.fieldOfView * Mathf.Deg2Rad;
            return (0.5f * (float) camera.pixelHeight) / Mathf.Tan(halfVerticalFOV);
        }

        /// <summary>
        /// Calculate field of view given the image size (width or height) and the focal length.
        /// Calculate the vertical field of view if height is provided, and the horizontal if width is provided.
        /// </summary>
        /// <param name="imageSize">The width or height of a camera image.</param>
        /// <param name="focalLength">The camera focal length.</param>
        public static float CalculateFieldOfView(int imageSize, float focalLength)
        {
            return 2.0f * Mathf.Atan(0.5f * (float) imageSize / focalLength);
        }

        /// <summary>
        /// Calculate image size (width or height) given the focal length and the field of view.
        /// Calculate the height if vertical field of view is provided, and the width if horizontal field of view is provided.
        /// </summary>
        /// <param name="focalLength">The camera focal length.</param>
        /// <param name="fieldOfView">The vertical or horizontal camera field of view.</param>
        public static int CalculateImageSize(float focalLength, float fieldOfView)
        {
            return (int) (2.0f * focalLength * Mathf.Tan(0.5f * fieldOfView));
        }

        /// <summary>
        /// Create RenderTexture pairs of different scaling levels.
        /// If the input requested texture size is larger than the effective camera size,
        /// use the requested size. Else, use the effective camera size.
        /// <param name="width">Requested texture width.</param>
        /// <param name="height">Requested texture height.</param>
        /// </summary>
        private void CreateRenderTextures(int width, int height)
        {
            // Calculate the render texture size based on the dual camera field of view
            // and the render camera focal length.

            var renderCameraLeftFocalLength = GetFocalLength(RenderCameraLeft);
            var renderCameraRightFocalLength = GetFocalLength(RenderCameraRight);

            var dualCameraLeftHorizontalFOV = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageWidth, (float) ViveSR_DualCameraImageCapture.FocalLengthLeft);
            var dualCameraLeftVerticalFOV = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageHeight, (float) ViveSR_DualCameraImageCapture.FocalLengthLeft);
            var dualCameraRightHorizontalFOV = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageWidth, (float) ViveSR_DualCameraImageCapture.FocalLengthRight);
            var dualCameraRightVerticalFOV = CalculateFieldOfView(ViveSR_DualCameraImageCapture.UndistortedImageHeight, (float) ViveSR_DualCameraImageCapture.FocalLengthRight);

            var renderTextureLeftWidth = CalculateImageSize(renderCameraLeftFocalLength, dualCameraLeftHorizontalFOV);
            var renderTextureLeftHeight = CalculateImageSize(renderCameraLeftFocalLength, dualCameraLeftVerticalFOV);
            var renderTextureRightWidth = CalculateImageSize(renderCameraRightFocalLength, dualCameraRightHorizontalFOV);
            var renderTextureRightHeight = CalculateImageSize(renderCameraRightFocalLength, dualCameraRightVerticalFOV);

            // If the input requested texture size is larger than the calculated value,
            // use the input value.

            if (renderTextureLeftWidth < width || renderTextureLeftHeight < height)
            {
                renderTextureLeftWidth = width;
                renderTextureLeftHeight = height;
            }
            if (renderTextureRightWidth < width || renderTextureRightHeight < height)
            {
                renderTextureRightWidth = width;
                renderTextureRightHeight = height;
            }

            // Create render textures of different scaling levels.
            var renderTextureDepth = 24;
            for (var i = 0; i < RenderTextureLevelsCount; ++i)
            {
                var scaleFactor = RenderTextureScaleFactors[i];
                var renderTextureLeft = new RenderTexture((int) (scaleFactor * renderTextureLeftWidth), (int) (scaleFactor * renderTextureLeftHeight), renderTextureDepth);
                var renderTextureRight = new RenderTexture((int) (scaleFactor * renderTextureRightWidth), (int) (scaleFactor * renderTextureRightHeight), renderTextureDepth);
                renderTextureLeft.name = "Render Texture (Left)";
                renderTextureRight.name = "Render Texture (Right)";
                renderTextureLeft.antiAliasing = 1;
                renderTextureRight.antiAliasing = 1;
                RenderTextureLevels[i].Left = renderTextureLeft;
                RenderTextureLevels[i].Right = renderTextureRight;
            }
        }

        private static ViveSR_VirtualCameraRig CreateVirtualCameraRig(Camera camera, RenderTexture renderTextureLeft,
            RenderTexture renderTextureRight, float verticalFOVLeft, float verticalFOVRight)
        {
            // Split the virtual camera into two cameras, and render to the render textures.

            // Create an empty game object for attaching ViveSR_VirtualCameraRig.
            var rigGameObject = new GameObject();

            // Add a component ViveSR_VirtualCameraRig to the game object.
            var virtualCameraRig = rigGameObject.AddComponent<ViveSR_VirtualCameraRig>() as ViveSR_VirtualCameraRig;

            // Initialize the component with the camera, the render textures, and the FOV.
            virtualCameraRig.Initialize(camera, renderTextureLeft, renderTextureRight,
                verticalFOVLeft, verticalFOVRight);

            // Output the created virtual camera rig.
            return virtualCameraRig;
        }
    }
}