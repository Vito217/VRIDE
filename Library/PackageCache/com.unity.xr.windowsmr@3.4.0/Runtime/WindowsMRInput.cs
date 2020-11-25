using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

using UnityEngine;
using UnityEngine.XR;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>Extension methods for XRInputSubsystem specific to WindowsMR</summary>
    public static class WindowsMRInput
    {
        public static NativeTypes.SpatialLocatability GetSpatialLocatability(this XRInputSubsystem input)
        {
            return Native.GetSpatialLocatability();
        }


        public struct SourceState
        {
            public int version;
            [MarshalAs(UnmanagedType.IUnknown)]
            public System.Object nativeState;
        }

        public static void GetCurrentSourceStates(this XRInputSubsystem input, List<System.Object> states)
        {
            if (states == null)
                return;

            states.Clear();
            int count = NativeApi.GetCountOfSourceStates();

            if (count > 0)
            {
                IntPtr[] ptrs = new IntPtr[count];
                NativeApi.GetAllSourceStates(ptrs, count);

                foreach (var ip in ptrs)
                {
                    if (ip != IntPtr.Zero)
                        states.Add(Marshal.GetObjectForIUnknown(ip));
                }

                NativeApi.ReleaseAllSourceStates(ptrs, states.Count);
            }
        }

        static class NativeApi
        {
        #if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
        #else
        #if ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
        #else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
        #endif
        #endif
            public static extern int GetCountOfSourceStates();

        #if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
        #else
        #if ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
        #else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
        #endif
        #endif
            public static extern void GetAllSourceStates([In, Out] IntPtr[] sourceStates, int countSourceStates);

        #if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
        #else
        #if ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
        #else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
        #endif
        #endif
            public static extern void ReleaseAllSourceStates([In, Out] IntPtr[] sourceStates, int countSourceStates);
        }
    }
}
