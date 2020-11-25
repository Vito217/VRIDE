using System;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Unity.XR.Oculus
{
    public static partial class Utils
    {
        /// <summary>
        /// Set the degree of foveation.  See [Oculus Documention](https://developer.oculus.com/documentation/quest/latest/concepts/mobile-ffr/).
        /// </summary>
        /// <param name="level">
        ///  level can be 0, 1, 2, 3, or 4:
        /// 
        /// * 0 disables multi-resolution
        /// * 1 low FFR setting
        /// * 2 medium FFR setting
        /// * 3 high FFR setting
        /// * 4 high top FFR setting
        /// </param>
        public static void SetFoveationLevel(int level)
        {
            IntPtr ovrJava = GetOvrJava();
            if (ovrJava == IntPtr.Zero)
            {
                Debug.LogError("Can't set foveation level");
                return;
            }
            vrapi_SetPropertyInt(ovrJava, OvrProperty.FoveationLevel, level);
        }

        /// <summary>
        /// Returns the degree of foveation.  See [Oculus Documentation](https://developer.oculus.com/documentation/quest/latest/concepts/mobile-ffr/).
        /// </summary>
        /// <returns>
        /// * -1 error retrieving foveation level
        /// * 0 disables multi-resolution
        /// * 1 low FFR setting
        /// * 2 medium FFR setting
        /// * 3 high FFR setting
        /// * 4 high top FFR setting
        /// </returns>
        public static int GetFoveationLevel()
        {
            IntPtr ovrJava = GetOvrJava();
            if (ovrJava == IntPtr.Zero ||
                !vrapi_GetPropertyInt(ovrJava, OvrProperty.FoveationLevel, out var ret))
            {
                Debug.LogError("Can't get foveation level");
                return -1;
            }
            return ret;
        }

        private enum OvrProperty
        {
            FoveationLevel = 15,
        }

        [DllImport("vrapi", EntryPoint = "vrapi_SetPropertyInt")]
        private static extern void vrapi_SetPropertyInt(IntPtr java, OvrProperty prop, int val);

        [DllImport("vrapi", EntryPoint = "vrapi_GetPropertyInt")]
        private static extern bool vrapi_GetPropertyInt(IntPtr java, OvrProperty propType, out int intVal);

        [DllImport("OculusXRPlugin")]
        private static extern IntPtr GetOvrJava();
    }
}
