//========= Copyright 2020, HTC Corporation. All rights reserved. ===========

namespace Vive.Plugin.SR
{
    /// <summary>
    /// Layer names. Store the constants in a class out of the UnityEditor namespace
    /// such that other UnityEngine scripts can use them in a build.
    /// </summary>
    public class ViveSR_Layers
    {
        #region Unity built-in layers
        public const string DefaultLayerName = "Default";
        public const string TransparentFXLayerName = "TransparentFX";
        public const string IgnoreRaycastLayerName = "Ignore Raycast";
        public const string WaterLayerName = "Water";
        public const string UILayerName = "UI";
        #endregion

        #region SRWorks layers
        public const string DualCameraLeftLayerName = "DualCamera (Left)";
        public const string DualCameraRightLayerName = "DualCamera (Right)";
        public const string VirtualWorldLayerName = "VirtualWorldLayer";
        public const string RenderPlaneLeftLayerName = "RenderPlane (Left)";
        public const string RenderPlaneRightLayerName = "RenderPlane (Right)";
        #endregion
    }
}
