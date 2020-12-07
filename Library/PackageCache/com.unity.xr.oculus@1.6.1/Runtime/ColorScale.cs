using UnityEngine;

namespace Unity.XR.Oculus
{
    public static partial class Utils
    {
        /// <summary>
        /// Set the color scale and color offset of the eye texture layers
        /// </summary>
        /// <param name="colorScale">Scales the eye layer texture color by this Vector4.</param>
        /// <param name="colorOffset">Offsets the eye layer texture color by this Vector4</param>
        public static void SetColorScaleAndOffset(Vector4 colorScale, Vector4 colorOffset)
        {
            NativeMethods.SetColorScale(colorScale.x, colorScale.y, colorScale.z, colorScale.w);
            NativeMethods.SetColorOffset(colorOffset.x, colorOffset.y, colorOffset.z, colorOffset.w);
        }
    }
}
