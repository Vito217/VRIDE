using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_TrackedCamera : MonoBehaviour
    {
        public DualCameraIndex CameraIndex;

        public Transform Anchor;
        public MeshRenderer DepthImageOccluder;
        public ViveSR_DualCameraImagePlane ImagePlane;
        public ViveSR_DualCameraImagePlane RenderPlane;
    }
}