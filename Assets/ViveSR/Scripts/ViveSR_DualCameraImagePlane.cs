//========= Copyright 2017, HTC Corporation. All rights reserved. ===========
using System.Runtime.InteropServices;
using UnityEngine;
using Vive.Plugin.SR.PassThrough;

namespace Vive.Plugin.SR
{
    [RequireComponent(typeof(MeshFilter))]
    public class ViveSR_DualCameraImagePlane : MonoBehaviour
    {
        // The index of the camera where the camera image comes from.
        public DualCameraIndex CameraIndex;
        public bool MirrorTextureVertically = false;

        [Header("Internal parameters")]
        public int DistortedImagePlaneWidth = 0;
        public int DistortedImagePlaneHeight = 0;
        public int UndistortedImagePlaneWidth = 0;
        public int UndistortedImagePlaneHeight = 0;
        public double DistortedImagePlaneCenterX = 0f;
        public double DistortedImagePlaneCenterY = 0f;
        public double UndistortedImagePlaneCenterX = 0f;
        public double UndistortedImagePlaneCenterY = 0f;
        public double CameraFocalLength = 0f;
        public float[] UndistortionedMap;
        public int CameraImagePlaneShrinkageRate = 4;
        private bool NeedSetShaderByDeviceType = false;
        private int num_vertex_rows = 0;
        private int num_vertex_cols = 0;

        private MeshRenderer mesh_rnd;
        private Material default_mat;
        private Mesh defaul_quad_mesh;
        private Mesh plane_mesh = null;
        private Mesh distorted_mesh = null;

        [HideInInspector]
        private UndistortionMethod undistort_method = UndistortionMethod.UNDISTORTED_BY_SRMODULE;
        public Texture2D UndistortedTexture = null;
        public Material UndistortedImagePlaneMaterial;

        private Vector3 image_planeInitial_local_position;
        private Quaternion image_planeInitial_local_rotation;
        private Vector3 image_planeInitial_local_scale;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable DistortedImageWidth has changed to DistortedImagePlaneWidth.
        * @warning The variable will remove in the future.
        */
        public int DistortedImageWidth
        {
            get { return DistortedImagePlaneWidth; }
            set { DistortedImagePlaneWidth = value; }
        }
        /**
        * The variable DistortedImageHeight has changed to DistortedImagePlaneHeight.
        * @warning The variable will remove in the future.
        */
        public int DistortedImageHeight
        {
            get { return DistortedImagePlaneHeight; }
            set { DistortedImagePlaneHeight = value; }
        }
        /**
        * The variable UndistortedImageWidth has changed to UndistortedImagePlaneWidth.
        * @warning The variable will remove in the future.
        */
        public int UndistortedImageWidth
        {
            get { return UndistortedImagePlaneWidth; }
            set { UndistortedImagePlaneWidth = value; }
        }
        /**
        * The variable UndistortedImageHeight has changed to UndistortedImagePlaneHeight.
        * @warning The variable will remove in the future.
        */
        public int UndistortedImageHeight
        {
            get { return UndistortedImagePlaneHeight; }
            set { UndistortedImagePlaneHeight = value; }
        }
        /**
        * The variable DistortedCx has changed to DistortedImagePlaneCenterX.
        * @warning The variable will remove in the future.
        */
        public double DistortedCx
        {
            get { return DistortedImagePlaneCenterX; }
            set { DistortedImagePlaneCenterX = value; }
        }
        /**
        * The variable DistortedCy has changed to DistortedImagePlaneCenterY.
        * @warning The variable will remove in the future.
        */
        public double DistortedCy
        {
            get { return DistortedImagePlaneCenterY; }
            set { DistortedImagePlaneCenterY = value; }
        }
        /**
        * The variable UndistortedCx has changed to UndistortedImagePlaneCenterX.
        * @warning The variable will remove in the future.
        */
        public double UndistortedCx
        {
            get { return UndistortedImagePlaneCenterX; }
            set { UndistortedImagePlaneCenterX = value; }
        }
        /**
        * The variable UndistortedCy has changed to UndistortedImagePlaneCenterY.
        * @warning The variable will remove in the future.
        */
        public double UndistortedCy
        {
            get { return UndistortedImagePlaneCenterY; }
            set { UndistortedImagePlaneCenterY = value; }
        }
        /**
        * The variable FocalLength has changed to CameraFocalLength.
        * @warning The variable will remove in the future.
        */
        public double FocalLength
        {
            get { return CameraFocalLength; }
            set { CameraFocalLength = value; }
        }
        /**
        * The variable UndistortionMap has changed to UndistortionedMap.
        * @warning The variable will remove in the future.
        */
        public float[] UndistortionMap
        {
            get { return UndistortionedMap; }
            set { UndistortionedMap = value; }
        }
        /**
        * The variable MeshResolutionShrinkingRatio has changed to CameraImagePlaneShrinkageRate.
        * @warning The variable will remove in the future.
        */
        public int MeshResolutionShrinkingRatio
        {
            get { return CameraImagePlaneShrinkageRate; }
            set { CameraImagePlaneShrinkageRate = value; }
        }
        /**
        * The variable undistortMap has changed to UndistortedTexture.
        * @warning The variable will remove in the future.
        */
        public Texture2D undistortMap
        {
            get { return UndistortedTexture; }
            set { UndistortedTexture = value; }
        }
        /**
        * The variable defishMat has changed to UndistortedImagePlaneMaterial.
        * @warning The variable will remove in the future.
        */
        public Material defishMat
        {
            get { return UndistortedImagePlaneMaterial; }
            set { UndistortedImagePlaneMaterial = value; }
        }

        #endregion


        /// <summary>
        /// Apply an additional local scale on the image plane.
        /// </summary>
        private float AdditionalScale = 1.0f;

        private bool ShaderInitialized = false;
        private bool need_set_gamma = false;

        /// <summary>Initialize an image plane.</summary>
        /// <param name="width">Image width</param>
        /// <param name="height">Image height</param>
        /// <param name="centerX">The x coordinate of the image center, or the principal point</param>
        /// <param name="centerY">The y coordinate of the image center, or the principal point</param>
        /// <param name="focalLength">Focal length</param>
        /// <param name="setShader">Whether to set the shader according to the device type</param>
        public void Initial(int width, int height, double centerX, double centerY, double focalLength, bool setShader)
        {
            // Store the input parameters.
            UndistortedImagePlaneWidth = width;
            UndistortedImagePlaneHeight = height;
            UndistortedImagePlaneCenterX = centerX;
            UndistortedImagePlaneCenterY = centerY;
            CameraFocalLength = focalLength;
            NeedSetShaderByDeviceType = setShader;

            StoreInitialLocalTransform();
            RestoreToInitialLocalTransform();

            if (ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
            {
                EnableNativeMeshManipulation(false);
            }

            defaul_quad_mesh = GetComponent<MeshFilter>().mesh;
            mesh_rnd = GetComponent<MeshRenderer>();
            if ( mesh_rnd ) default_mat = mesh_rnd.sharedMaterial;
            
            if (CameraImagePlaneShrinkageRate < 1)
                CameraImagePlaneShrinkageRate = 1;
            num_vertex_rows = DistortedImagePlaneHeight / CameraImagePlaneShrinkageRate;
            num_vertex_cols = DistortedImagePlaneWidth / CameraImagePlaneShrinkageRate;

            if (undistort_method == UndistortionMethod.UNDISTORTED_BY_MESH)
            {
                CreateMesh(num_vertex_rows, num_vertex_cols);
            }
            else if (undistort_method == UndistortionMethod.UNDISTORTED_BY_SRMODULE)
            {
                if (plane_mesh == null)
                {
                    SetCorrectSize();
                    SetUV(CameraIndex);
                }

                if (ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
                {
                    InitializeNativeMeshManipulation();
                    SetLocalTransformToIdentity();
                    if (gameObject.activeInHierarchy)
                    {
                        EnableNativeMeshManipulation(true);
                    }
                }
            }
        }

        public void SetUndistortMethod( UndistortionMethod NewMethod )
        {
            if (undistort_method == NewMethod) return;

            if (ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
            {
                EnableNativeMeshManipulation(false);

                if (undistort_method == UndistortionMethod.UNDISTORTED_BY_SRMODULE)
                {
                    RestoreToInitialLocalTransform();
                }
            }

            undistort_method = NewMethod;
            if (undistort_method == UndistortionMethod.UNDISTORTED_BY_MESH)
            {
                if (distorted_mesh == null)
                    CreateMesh(num_vertex_rows, num_vertex_cols);
                else
                    GetComponent<MeshFilter>().sharedMesh = distorted_mesh;
            }
            else
            {
                if (plane_mesh == null)
                {
                    SetCorrectSize();
                    SetUV(CameraIndex);
                }
                else
                {
                    GetComponent<MeshFilter>().sharedMesh = plane_mesh;
                }
                
                if (undistort_method == UndistortionMethod.UNDISTORTED_BY_SRMODULE &&
                    ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
                {
                    InitializeNativeMeshManipulation();
                    SetLocalTransformToIdentity();
                    if (gameObject.activeInHierarchy)
                    {
                        EnableNativeMeshManipulation(true);
                    }
                }
            }

            if ( mesh_rnd )
                mesh_rnd.sharedMaterial = default_mat;
        }

        /// <summary>
        /// Set the main texture of the attached game object.
        /// </summary>
        public void SetMainTexture(Texture texture)
        {
            GetComponent<Renderer>().material.mainTexture = texture;
        }

        /// <summary>
        /// Set the shader.
        /// </summary>
        public void SetShader(Shader shader)
        {
            GetComponent<Renderer>().material.shader = shader;
        }

        /// <summary>
        /// Set the additional local scale on the image plane.
        /// The scale is applied after calling Initial().
        /// </summary>
        public void SetAdditionalScale(float scale)
        {
            AdditionalScale = scale;
        }

        void Start()
        {
            // Disable unnecessary renderer settings.
            var renderer = GetComponent<MeshRenderer>() as MeshRenderer;
            renderer.lightProbeUsage = UnityEngine.Rendering.LightProbeUsage.Off;
            renderer.reflectionProbeUsage = UnityEngine.Rendering.ReflectionProbeUsage.Off;
            renderer.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
            renderer.receiveShadows = false;
            renderer.motionVectorGenerationMode = MotionVectorGenerationMode.ForceNoMotion;
        }

        void OnEnable()
        {
            if (undistort_method == UndistortionMethod.UNDISTORTED_BY_SRMODULE &&
                ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
            {
                EnableNativeMeshManipulation(true);
            }
        }

        void OnDisable()
        {
            if (ViveSR_DualCameraImageCapture.UndistortTextureIsNative)
            {
                EnableNativeMeshManipulation(false);
            }
        }

        private void InitDefishedMap()
        {
            float[] defishedUV = new float[DistortedImagePlaneWidth * DistortedImagePlaneHeight * 2];
            byte[] defishedUV_byte = new byte[DistortedImagePlaneWidth * DistortedImagePlaneHeight * 8];

            for (int i = 0; i < DistortedImagePlaneWidth; ++i)
            {
                for (int j = 0; j < DistortedImagePlaneHeight; ++j)
                {
                    int idx = j * DistortedImagePlaneWidth + i;
                    defishedUV[idx * 2 + 0] = UndistortionedMap[idx * 4 + 0] / DistortedImagePlaneWidth;
                    defishedUV[idx * 2 + 1] = UndistortionedMap[idx * 4 + 1] / DistortedImagePlaneHeight;
                }
            }

            System.Buffer.BlockCopy(defishedUV, 0, defishedUV_byte, 0, defishedUV_byte.Length);
            UndistortedTexture = new Texture2D(DistortedImagePlaneWidth, DistortedImagePlaneHeight, TextureFormat.RGFloat, false);            
            UndistortedTexture.LoadRawTextureData(defishedUV_byte); UndistortedTexture.Apply();

            if (UndistortedImagePlaneMaterial) UndistortedImagePlaneMaterial.SetTexture("_DefishTex", UndistortedTexture);
        }
        private void Update()
        {
            if (!NeedSetShaderByDeviceType)
                return;

            // Set shader.
            if (!ShaderInitialized)
            {
                mesh_rnd = GetComponent<MeshRenderer>();
                if (mesh_rnd)
                    default_mat = mesh_rnd.sharedMaterial;

                if (!ViveSR.UpdateUnityPassThrough)
                    return;

                int deviceType = (int)VRDevice.None;
                SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.DEVICE_SYSTEM_TYPE, ref deviceType);
                if (VRDevice.COSMOS != (VRDevice)deviceType || PassThrough.SRWork_PassThrough.Image4kReady)
                {
                    default_mat.shader = Shader.Find("ViveSR/Unlit, Textured, Stencil");
                }
                else
                {
                    default_mat.shader = Shader.Find("ViveSR/depurpleShader");
                    need_set_gamma = true;
                }
                ShaderInitialized = true;
            }
            if (need_set_gamma)//run every frame
            {
                float gamma = PassThrough.SRWork_PassThrough.pass_through_data_.gamma;
                if (gamma > 0.0f)
                    default_mat.SetFloat("_gamma", gamma);
            }
        }

        /// <summary>
        /// Create a mesh to display the camera image.
        /// </summary>
        private void CreateMesh(int num_vertex_rows, int num_vertex_cols)
        {
            // Mesh topology, where (i,j) is the row-major vertex index:
            //  +--> x
            //  |
            //  v    (0,0)--(0,1)--(0,2)-- ..
            //  -y     |   /  |   /  |   /
            //         |  /   |  /   |  /
            //       (1,0)--(1,1)--(1,2)-- ..
            //         |   /  |   /  |   /
            //         ..     ..     ..

            // Create vertices, set UVs, and create normals.
            UndistortMesh();            

            // Create triangles.
            int[] triangles = new int[(num_vertex_rows - 1) * (num_vertex_cols - 1) * 2 * 3];
            for (int i = 0; i < num_vertex_rows - 1; ++i)
            {
                for (int j = 0; j < num_vertex_cols - 1; ++j)
                {
                    int triangle_index = ((num_vertex_cols - 1) * i + j) * 2 * 3;
                    int vertex00_index = num_vertex_cols * i + j;
                    int vertex01_index = vertex00_index + 1;
                    int vertex10_index = vertex00_index + num_vertex_cols;
                    int vertex11_index = vertex10_index + 1;
                    triangles[triangle_index + 0] = vertex00_index;
                    triangles[triangle_index + 1] = vertex01_index;
                    triangles[triangle_index + 2] = vertex10_index;
                    triangles[triangle_index + 3] = vertex10_index;
                    triangles[triangle_index + 4] = vertex01_index;
                    triangles[triangle_index + 5] = vertex11_index;
                }
            }

            // Set the mesh by these mesh components.
            distorted_mesh.triangles = triangles;
            GetComponent<MeshFilter>().sharedMesh = distorted_mesh;
        }

        /// <summary>
        /// Deform the image plane mesh according to the undistortion map in order to undistort the camera image.
        /// </summary>
        private void UndistortMesh()
        {
            // Transform from pixel space to image plane space.
            // Pixel space:
            //     origin: the upper-left corner of the image
            //     x axis: the local x axis
            //     y axis: the local y axis
            //     z axis: the local z axis
            //     unit: 1 pixel
            // Image plane space:
            //     origin: the principal point of the image scaled from pixel-unit to meter-unit
            //     x axis: the local x axis
            //     y axis: the local y axis
            //     z axis: the local z axis
            //     unit: 1 meter

            // Calculate the length of a pixel in the real space unit.
            float image_plane_distance = transform.localPosition.z;
            float pixel_length = image_plane_distance / (float)CameraFocalLength;

            // Create vectors with zero z values.
            // Convert from right-handed coordinates to the left-handed coordinates.
            // Also create uvs.
            Vector3[] pixel_vertices = new Vector3[num_vertex_rows * num_vertex_cols];
            Vector2[] uvs = new Vector2[num_vertex_rows * num_vertex_cols];
            Vector3[] normals = new Vector3[num_vertex_rows * num_vertex_cols];
            for (int i = 0; i < num_vertex_rows; ++i)
            {
                for (int j = 0; j < num_vertex_cols; ++j)
                {
                    int undistortioned_map_index = (i * CameraImagePlaneShrinkageRate * DistortedImagePlaneWidth + j * CameraImagePlaneShrinkageRate) * 4;
                    float distorted_x = (j * CameraImagePlaneShrinkageRate) + 0.5f;
                    float distorted_y = (i * CameraImagePlaneShrinkageRate) + 0.5f;
                    float undistorted_x = UndistortionedMap[undistortioned_map_index + 2] - (float)UndistortedImagePlaneCenterX;
                    float undistorted_y = UndistortionedMap[undistortioned_map_index + 3] - (float)UndistortedImagePlaneCenterY;
                    pixel_vertices[i * num_vertex_cols + j] = new Vector3(undistorted_x * pixel_length, -1 * undistorted_y * pixel_length, 0f);
                    uvs[i * num_vertex_cols + j] = new Vector2(distorted_x / (float) DistortedImagePlaneWidth, distorted_y / (float) DistortedImagePlaneHeight);
                    normals[i * num_vertex_cols + j] = new Vector3(0.0f, 0.0f, -1.0f);
                }
            }

            distorted_mesh = new Mesh();
            distorted_mesh.vertices = pixel_vertices;
            distorted_mesh.uv = uvs;
            distorted_mesh.normals = normals;
        }

        /// <summary>
        /// Set the scale of the image plane according to the image size and the focal length.
        /// </summary>
        private void SetCorrectSize()
        {
            float image_width = UndistortedImagePlaneWidth;
            float image_height = UndistortedImagePlaneHeight;
            float image_aspect_ratio = image_width / image_height;

            // Get the distance of the image plane to the camera.
            // ASSUME the plane is in the z direction of the camera.
            float image_plane_disance_z = transform.localPosition.z;
            float focal_length = (float)CameraFocalLength;

            // Calculate the correct size of the image plane according to the size of
            // the original images, the image plane distance and the focal length.
            plane_mesh = Instantiate(defaul_quad_mesh);    // Copy from the default quad mesh.
            Vector3[] original_vertices = plane_mesh.vertices;  // Get a copy of the vertices of the plane.
            Vector3 upper_right_most_vertex = new Vector3(-1e8f, -1e8f, 0);
            Vector3 lower_left_most_vertex = new Vector3(1e8f, 1e8f, 0);
            for (int i = 0; i < original_vertices.Length; i++)
            {
                upper_right_most_vertex = Vector3.Max(upper_right_most_vertex, original_vertices[i]);
                lower_left_most_vertex = Vector3.Min(lower_left_most_vertex, original_vertices[i]);
            }
            float image_plane_width = upper_right_most_vertex.x - lower_left_most_vertex.x;
            float image_plane_height = upper_right_most_vertex.y - lower_left_most_vertex.y;
            float image_plane_aspect_ratio = image_plane_width / image_plane_height;
            // Create the transformation matrices.
            // Translate to the geometric center.
            Vector3 geometric_center = (upper_right_most_vertex + lower_left_most_vertex) / 2;
            Matrix4x4 translation_to_geomatric_center = Matrix4x4.TRS(-1 * geometric_center, Quaternion.identity, Vector3.one);
            // Scale x and y to fit the vertical FOV.
            float fov_scale_factor = ((image_height / focal_length) * image_plane_disance_z) / image_plane_height;
            Matrix4x4 scaling_for_correct_fov = Matrix4x4.TRS(Vector3.zero, Quaternion.identity, new Vector3(fov_scale_factor, fov_scale_factor, 1));
            // Scale x to fit the aspect ratio.
            float aspect_ratio_scale_factor = image_aspect_ratio / image_plane_aspect_ratio;
            Matrix4x4 scaling_for_correct_aspect_ratio = Matrix4x4.TRS(Vector3.zero, Quaternion.identity, new Vector3(aspect_ratio_scale_factor, 1, 1));
            // Apply an additional scale.
            Matrix4x4 additional_scaling = Matrix4x4.TRS(Vector3.zero, Quaternion.identity, new Vector3(AdditionalScale, AdditionalScale, 1));
            // Translate back to the origin by the inverse matrix.
            // Combine all transformations.
            Matrix4x4 transformation_for_original_image = translation_to_geomatric_center.inverse * additional_scaling * scaling_for_correct_aspect_ratio * scaling_for_correct_fov * translation_to_geomatric_center;
            // Apply the transformation.
            for (int i = 0; i < original_vertices.Length; i++)
            {
                original_vertices[i] = transformation_for_original_image.MultiplyPoint3x4(original_vertices[i]);
            }
            // Assign the vertices for the correct image plane size.
            plane_mesh.vertices = original_vertices;            
        }

        /// <summary>
        /// Mirror the uv because the origin point of images of Unity is different from the source image.
        /// </summary>
        /// <param name="eye"></param>
        private void SetUV(DualCameraIndex eye)
        {
            if (MirrorTextureVertically)
            {
                Vector2[] src_uv = plane_mesh.uv;
                Vector2[] dst_uv = new Vector2[src_uv.Length];
                for (int i = 0; i < src_uv.Length; i++)
                    dst_uv[i] = new Vector2(src_uv[i].x, src_uv[src_uv.Length - i - 1].y);
                plane_mesh.uv = dst_uv;
            }

            GetComponent<MeshFilter>().sharedMesh = plane_mesh;
        }

        /// <summary>
        /// Let the native plugin able to modify the mesh vertex buffer.
        /// Assume the mesh contains only vertices components.
        /// The vertex position of the mesh will be modified.
        /// </summary>
        private void InitializeNativeMeshManipulation()
        {
            Mesh mesh = GetComponent<MeshFilter>().mesh;

            // Get the vertices if the origin of the vertices was at TrackedCameraObject.
            Transform object_to_world_transform = transform;
            ViveSR_TrackedCamera tracked_camera;
            if (CameraIndex == DualCameraIndex.LEFT)
            {
                tracked_camera = ViveSR_DualCameraRig.Instance.TrackedCameraLeft;
            }
            else
            {
                tracked_camera = ViveSR_DualCameraRig.Instance.TrackedCameraRight;
            }
            Transform camera_to_world_transform = tracked_camera.transform;
            Vector3[] Vertices = mesh.vertices;
            for (int i = 0; i < Vertices.Length; ++i)
            {
                // Transform the vertex from the object space to the camera space.
                Vertices[i] = camera_to_world_transform.InverseTransformPoint(object_to_world_transform.TransformPoint(Vertices[i]));
            }

            // Send mesh buffers to the plugin.
            mesh.MarkDynamic();
            
            // Set the mesh local bounding box large enough such that the mesh is always visible.
            Bounds bounds = mesh.bounds;
            bounds.extents = new Vector3(100f, 100f, 100f);
            mesh.bounds = bounds;
        }

        private void EnableNativeMeshManipulation(bool enable)
        {
        }

        private void StoreInitialLocalTransform()
        {
            image_planeInitial_local_position = transform.localPosition;
            image_planeInitial_local_rotation = transform.localRotation;
            image_planeInitial_local_scale = transform.localScale;
        }

        /// <summary>
        /// Move the image plane as a child of game object DualCamera (head),
        /// and set the local transform of the image plane to identity.
        /// </summary>
        private void SetLocalTransformToIdentity()
        {
            transform.parent = ViveSR_DualCameraRig.Instance.transform;
            transform.localPosition = Vector3.zero;
            transform.localRotation = Quaternion.identity;
            transform.localScale = Vector3.one;
        }

        /// <summary>
        /// Move the image plane as a child of game object Anchor (left/right),
        /// and set the local transform of the image plane to the initial value.
        /// </summary>
        private void RestoreToInitialLocalTransform()
        {
            transform.localPosition = image_planeInitial_local_position;
            transform.localRotation = image_planeInitial_local_rotation;
            transform.localScale = image_planeInitial_local_scale;
        }
        private void Release() {
            Texture2D.Destroy(UndistortedTexture);
            UndistortedTexture = null;
        }
        void OnApplicationQuit()
        {
            if (ShaderInitialized)
            {
                default_mat.shader = Shader.Find("ViveSR/Unlit, Textured, Stencil");
            }
        }
    }
}