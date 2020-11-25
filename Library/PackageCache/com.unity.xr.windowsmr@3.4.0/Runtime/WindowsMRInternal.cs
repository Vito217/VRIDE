using System;
using System.IO;

using UnityEngine;
using UnityEngine.Scripting;

#if UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || UNITY_WINRT
using System.Runtime.InteropServices;
#endif

namespace UnityEngine.XR.WindowsMRInternals
{
    [Preserve]
    internal class WindowsMRInternal
    {
        static WindowsMRInternal()
        {
#if UNITY_EDITOR_WIN
            string pluginFolderPathBase = Path.GetFullPath("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64");
#else
            string pluginFolderPathBase = "";
#endif

            UnityWindowsMR_EmulationLibs_SetPluginFolderPaths(pluginFolderPathBase);
        }

        internal static void Init()
        {
        }

#if UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || UNITY_WINRT
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Ansi)]
        public static extern void UnityWindowsMR_EmulationLibs_SetPluginFolderPaths([MarshalAs(UnmanagedType.LPWStr)] string pluginFolderPath_x86_64);
#else
        static void UnityWindowsMR_EmulationLibs_SetPluginFolderPaths(string pluginFolderPath_x86_64)
        {
        }
#endif
    }
}
