using System.Runtime.InteropServices;

using UnityEngine.Scripting;
using UnityEngine.XR.ARSubsystems;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// The Windows MR implementation of the <c>XRSessionSubsystem</c>. Do not create this directly.
    /// Use <c>WindowsMRSessionSubsystemDescriptor.Create()</c> instead.
    /// </summary>
    [Preserve]
    public sealed class WindowsMRSessionSubsystem : XRSessionSubsystem
    {
        protected override Provider CreateProvider()
        {
            return new WindowsMRProvider();
        }

        class WindowsMRProvider : Provider
        {
            public WindowsMRProvider()
            {
                NativeApi.UnityWindowsMR_session_construct();
            }

            public override Promise<SessionAvailability> GetAvailabilityAsync()
            {
                var availability =
                SessionAvailability.Installed | SessionAvailability.Supported;
                return Promise<SessionAvailability>.CreateResolvedPromise(availability);
            }

            public override TrackingState trackingState
            {
                get
                {
                    return NativeApi.UnityWindowsMR_session_getTrackingState();
                }
            }

            public override NotTrackingReason notTrackingReason
            {
                get
                {
                    return NativeApi.UnityWindowsMR_session_getNotTrackingReason();
                }
            }

            public override void Destroy()
            {
                NativeApi.UnityWindowsMR_session_destroy();
            }
            public override void Pause()
            {
                NativeApi.UnityWindowsMR_session_pause();
            }

            public override void Resume()
            {
                NativeApi.UnityWindowsMR_session_resume();
            }

            public override void Reset()
            {
                NativeApi.UnityWindowsMR_session_reset();
            }

        }

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.SubsystemRegistration)]
        static void RegisterDescriptor()
        {
            XRSessionSubsystemDescriptor.RegisterDescriptor(new XRSessionSubsystemDescriptor.Cinfo
            {
                id = "Windows Mixed Reality Session",
                subsystemImplementationType = typeof(WindowsMRSessionSubsystem),
                supportsInstall = false
            });
        }

        static class NativeApi
        {
#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_session_construct();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_session_destroy();


#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_session_pause();


#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_session_resume();


#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_session_reset();


#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern TrackingState UnityWindowsMR_session_getTrackingState();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern NotTrackingReason UnityWindowsMR_session_getNotTrackingReason();
        }
    }
}
