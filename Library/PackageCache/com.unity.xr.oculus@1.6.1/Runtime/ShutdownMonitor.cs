#if UNITY_STANDALONE_WIN && !UNITY_EDITOR
#define OCULUSPLUGIN_WINDOWS_PLATFORM_ONLY
#endif

#if OCULUSPLUGIN_WINDOWS_PLATFORM_ONLY
using System;
using System.Runtime.InteropServices;
using UnityEngine;
#endif

namespace Unity.XR.Oculus
{
    internal static class ShutdownMonitor
    {
#if OCULUSPLUGIN_WINDOWS_PLATFORM_ONLY
        internal static void Initialize()
        {
            if (!NativeMethods.IsOculusXRModuleLoaded())
                return;

            Application.onBeforeRender += Update;
        }

        internal static void Deinitialize()
        {
            Application.onBeforeRender -= Update;
        }

        private static void Update()
        {
            if (NativeMethods.GetAppShouldQuit())
            {
                Application.Quit();
            }
        }
#else
        internal static void Initialize() { }
        internal static void Deinitialize() { }
#endif // UNITY_SHUTDOWNMONITOR_ENABLE
    }
}
