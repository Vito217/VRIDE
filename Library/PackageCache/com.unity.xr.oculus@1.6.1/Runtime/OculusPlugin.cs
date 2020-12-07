#if !(UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || (UNITY_ANDROID && !UNITY_EDITOR))
#define OCULUSPLUGIN_UNSUPPORTED_PLATFORM
#endif

#if (UNITY_ANDROID && !UNITY_EDITOR)
#define OCULUSPLUGIN_ANDROID_PLATFORM_ONLY
#endif

using System;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Unity.XR.Oculus
{
    public static partial class NativeMethods
    {
        [StructLayout(LayoutKind.Sequential)]
        internal struct UserDefinedSettings
        {
            public ushort sharedDepthBuffer;
            public ushort dashSupport;
            public ushort stereoRenderingMode;
            public ushort colorSpace;
            public ushort lowOverheadMode;
            public ushort protectedContext;
            public ushort focusAware;
            public ushort optimizeBufferDiscards;
        }

        internal enum OvrProperty
        {
            FoveationLevel = 15,
        }

        internal static void SetColorScale(float x, float y, float z, float w)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.SetColorScale(x, y, z, w);
#endif
        }

        internal static void SetColorOffset(float x, float y, float z, float w)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.SetColorOffset(x, y, z, w);
#endif
        }

        internal static IntPtr GetOvrJava()
        {
#if OCULUSPLUGIN_ANDROID_PLATFORM_ONLY
            return Internal.GetOvrJava();
#else
            return IntPtr.Zero;
#endif
        }

        internal static void SetPropertyInt(IntPtr java, OvrProperty prop, int val)
        {
#if OCULUSPLUGIN_ANDROID_PLATFORM_ONLY
            Internal.vrapi_SetPropertyInt(java, prop, val);
#endif
        }

        internal static bool GetPropertyInt(IntPtr java, OvrProperty propType, out int intVal)
        {
#if OCULUSPLUGIN_ANDROID_PLATFORM_ONLY
            return Internal.vrapi_GetPropertyInt(java, propType, out intVal);
#else
            intVal = -1;
            return false;
#endif
        }

        internal static bool GetIsSupportedDevice()
        {
#if OCULUSPLUGIN_ANDROID_PLATFORM_ONLY
            return Internal.GetIsSupportedDevice();
#else
            return false;
#endif
        }

        internal static bool LoadOVRPlugin(string ovrpPath)
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return false;
#else
            return Internal.LoadOVRPlugin(ovrpPath);
#endif
        }

        internal static void UnloadOVRPlugin()
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.UnloadOVRPlugin();
#endif
        }

        internal static void SetUserDefinedSettings(UserDefinedSettings settings)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.SetUserDefinedSettings(settings);
#endif
        }

        internal static int SetCPULevel(int cpuLevel)
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return -1;
#else
            return Internal.SetCPULevel(cpuLevel);
#endif
        }

        internal static int SetGPULevel(int gpuLevel)
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return -1;
#else
            return Internal.SetGPULevel(gpuLevel);
#endif
        }

        internal static void GetOVRPVersion(byte[] version)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.GetOVRPVersion(version);
#endif
        }

        internal static void EnablePerfMetrics(bool enable)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.EnablePerfMetrics(enable);
#endif
        }

        internal static void EnableAppMetrics(bool enable)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.EnableAppMetrics(enable);
#endif
        }

        internal static bool SetDeveloperModeStrict(bool active)
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return false;
#else
            return Internal.SetDeveloperModeStrict(active);
#endif
        }

        internal static bool GetBoundaryConfigured()
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return false;
#else
            return Internal.GetBoundaryConfigured();
#endif
        }

        internal static bool GetBoundaryDimensions(Boundary.BoundaryType boundaryType, out Vector3 dimensions)
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            dimensions = Vector3.zero;
            return false;
#else
            return Internal.GetBoundaryDimensions(boundaryType, out dimensions);
#endif
        }

        internal static bool GetBoundaryVisible()
        {
#if OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return false;
#else
            return Internal.GetBoundaryVisible();
#endif
        }

        internal static void SetBoundaryVisible(bool boundaryVisible)
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            Internal.SetBoundaryVisible(boundaryVisible);
#endif
        }

        internal static bool GetAppShouldQuit()
        {
#if !OCULUSPLUGIN_UNSUPPORTED_PLATFORM
            return Internal.GetAppShouldQuit();
#else
            return false;
#endif
        }

        internal static bool IsOculusXRModuleLoaded()
        {
            var found = true;
            try
            {
                var pluginVersion = Stats.PluginVersion;
            }
            catch (System.Exception)
            {
                Debug.LogError("Unable to find the OculusXRPlugin DLL\n");
                found = false;
            }
            return found;
        }


        private static class Internal
        {
            [DllImport("OculusXRPlugin")]
            internal static extern void SetColorScale(float x, float y, float z, float w);

            [DllImport("OculusXRPlugin")]
            internal static extern void SetColorOffset(float x, float y, float z, float w);

            [DllImport("OculusXRPlugin")]
            internal static extern IntPtr GetOvrJava();

            [DllImport("OculusXRPlugin")]
            internal static extern bool GetIsSupportedDevice();

            [DllImport("OculusXRPlugin", CharSet=CharSet.Ansi)]
            internal static extern bool LoadOVRPlugin(string ovrpPath);

            [DllImport("OculusXRPlugin")]
            internal static extern void UnloadOVRPlugin();

            [DllImport("OculusXRPlugin")]
            internal static extern void SetUserDefinedSettings(UserDefinedSettings settings);

            [DllImport("OculusXRPlugin")]
            internal static extern int SetCPULevel(int cpuLevel);

            [DllImport("OculusXRPlugin")]
            internal static extern int SetGPULevel(int gpuLevel);

            [DllImport("vrapi", EntryPoint = "vrapi_SetPropertyInt")]
            internal static extern void vrapi_SetPropertyInt(IntPtr java, OvrProperty prop, int val);

            [DllImport("vrapi", EntryPoint = "vrapi_GetPropertyInt")]
            internal static extern bool vrapi_GetPropertyInt(IntPtr java, OvrProperty propType, out int intVal);

            [DllImport("OculusXRPlugin", CharSet=CharSet.Auto)]
            internal static extern void GetOVRPVersion(byte[] version);

            [DllImport("OculusXRPlugin")]
            internal static extern void EnablePerfMetrics(bool enable);

            [DllImport("OculusXRPlugin")]
            internal static extern void EnableAppMetrics(bool enable);

            [DllImport("OculusXRPlugin")]
            internal static extern bool SetDeveloperModeStrict(bool active);

            [DllImport("OculusXRPlugin")]
            internal static extern bool GetBoundaryConfigured();

            [DllImport("OculusXRPlugin")]
            internal static extern bool GetBoundaryDimensions(Boundary.BoundaryType boundaryType, out Vector3 dimensions);

            [DllImport("OculusXRPlugin")]
            internal static extern bool GetBoundaryVisible();

            [DllImport("OculusXRPlugin")]
            internal static extern void SetBoundaryVisible(bool boundaryVisible);

            [DllImport("OculusXRPlugin")]
            internal static extern bool GetAppShouldQuit();
        }
    }
}
