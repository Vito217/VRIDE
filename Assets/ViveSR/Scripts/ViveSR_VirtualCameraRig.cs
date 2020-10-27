//========= Copyright 2020, HTC Corporation. All rights reserved. ===========
using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_VirtualCameraRig : MonoBehaviour
    {
        public Camera CameraLeft {get; private set;}
        public Camera CameraRight {get; private set;}

        private bool Initialized = false;

        /// <summary>
        /// Clone the input camera into two cameras, set the camera parameters, and
        /// make the two camera render to the render textures.
        /// </summary>
        /// <param name="verticalFOVLeft">The vertical field of view of the left camera in degrees.</param>
        /// <param name="verticalFOVRight">The vertical field of view of the right camera in degrees.</param>
        public void Initialize(Camera camera, RenderTexture renderTextureLeft, RenderTexture renderTextureRight,
            float verticalFOVLeft, float verticalFOVRight)
        {
            if (Initialized)
            {
                return;
            }

            // If the game object that the camera is attached to is (not) active,
            // set the game object (not) active.
            gameObject.SetActive(camera.gameObject.activeInHierarchy);

            // Clone the camera.
            CameraLeft = camera;
            CameraRight = GameObject.Instantiate(CameraLeft);

            // Destroy the cloned child game objects.
            DestroyAllChildGameObjects(CameraRight.transform);

            // Set the hierarchy.
            transform.parent = CameraLeft.transform.parent;
            CameraLeft.transform.parent = transform;
            CameraRight.transform.parent = transform;

            // Set identity local transformation.
            transform.localPosition = Vector3.zero;
            transform.localRotation = Quaternion.identity;
            transform.localScale = Vector3.one;
            CameraLeft.transform.localPosition = Vector3.zero;
            CameraLeft.transform.localRotation = Quaternion.identity;
            CameraLeft.transform.localScale = Vector3.one;
            CameraRight.transform.localPosition = Vector3.zero;
            CameraRight.transform.localRotation = Quaternion.identity;
            CameraRight.transform.localScale = Vector3.one;

            // Set the names.
            var originalName = CameraLeft.name;
            gameObject.name = originalName + " (Rig)";
            CameraLeft.name = originalName + " (Left)";
            CameraRight.name = originalName + " (Right)";

            // Set the camera target textures to the render textures.
            // Now the cameras are not VR cameras.
            CameraLeft.targetTexture = renderTextureLeft;
            CameraRight.targetTexture = renderTextureRight;

            // Set the camera vertical FOV.
            CameraLeft.fieldOfView = verticalFOVLeft;
            CameraRight.fieldOfView = verticalFOVRight;

            // Set the clear flags to Don't Clear.
            CameraLeft.clearFlags = CameraClearFlags.Nothing;
            CameraRight.clearFlags = CameraClearFlags.Nothing;

            // Clear the culling mask of the image planes and the render planes.
            var layerMask = LayerMask.GetMask(
                ViveSR_Layers.DualCameraLeftLayerName,
                ViveSR_Layers.DualCameraRightLayerName,
                ViveSR_Layers.RenderPlaneLeftLayerName,
                ViveSR_Layers.RenderPlaneRightLayerName);
            CameraLeft.cullingMask &= ~layerMask;
            CameraRight.cullingMask &= ~layerMask;

            // Disable HDR.
            CameraLeft.allowHDR = false;
            CameraRight.allowHDR = false;

            // Disable MSAA.
            CameraLeft.allowMSAA = false;
            CameraRight.allowMSAA = false;

            // If the original camera (the left camera) has an audio listener,
            // destroy the audio listener of the cloned camera (the right camera).
            if (CameraLeft.GetComponent<AudioListener>())
            {
                Destroy(CameraRight.GetComponent<AudioListener>());
            }

            Initialized = true;
        }

        /// <summary>
        /// Update the poses of the left camera and the right camera.
        /// If the attaching game object is not active, do not update the poses.
        /// </summary>
        public void SetCameraPoses(Vector3 positionLeft, Quaternion rotationLeft, Vector3 positionRight, Quaternion rotationRight)
        {
            if (!gameObject.activeInHierarchy)
            {
                return;
            }

            CameraLeft.transform.localPosition = positionLeft;
            CameraLeft.transform.localRotation = rotationLeft;
            CameraRight.transform.localPosition = positionRight;
            CameraRight.transform.localRotation = rotationRight;
        }

        /// <summary>
        /// Set the game object, as well as the left/right camera game object, active or not active.
        /// </sumamry>
        public void SetGameObjectsActive(bool active)
        {
            gameObject.SetActive(active);
            CameraLeft.gameObject.SetActive(active);
            CameraRight.gameObject.SetActive(active);
        }

        /// <summary>
        /// Set the Camera.enabled of the left and right camera.
        /// </summary>
        public void EnableCameras(bool enable)
        {
            CameraLeft.enabled = enable;
            CameraRight.enabled = enable;
        }

        /// <summary>
        /// Set the camera target textures.
        /// </summary>
        public void SetTargetTextures(RenderTexture renderTextureLeft, RenderTexture renderTextureRight)
        {
            CameraLeft.targetTexture = renderTextureLeft;
            CameraRight.targetTexture = renderTextureRight;
        }

        private void DestroyAllChildGameObjects(Transform transform)
        {
            // Get all children.
            var childCount = transform.childCount;
            Transform[] children = new Transform[childCount];
            for (var i = 0; i < childCount; ++i)
            {
                children[i] = transform.GetChild(i);
            }

            // Destroy the game object of each child.
            foreach (var child in children)
            {
                Destroy(child.gameObject);
            }
        }
    }
}