using System;
using Unity.Collections;
using Unity.Collections.LowLevel.Unsafe;
using System.Runtime.InteropServices;
using UnityEngine.Scripting;
using UnityEngine.XR.InteractionSubsystems;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// WindowsMR implementation of the <c>XRGestureSubsystem</c>. Do not create this directly. Use the <c>SubsystemManager</c> instead.
    /// </summary>
    [Preserve]
    public sealed class WindowsMRGestureSubsystem : XRGestureSubsystem
    {
        /// <summary>
        /// A collection of all WindowsMRHoldGestureEvents managed by this subsystem.
        /// This is cleared every frame and refreshed with new gesture events.
        /// </summary>
        public NativeArray<WindowsMRHoldGestureEvent> holdGestureEvents { get { return m_WindowsMRProvider.holdGestureEvents; } }

        /// <summary>
        /// A collection of all WindowsMRManipulationGestureEvent managed by this subsystem.
        /// This is cleared every frame and refreshed with new gesture events.
        /// </summary>
        public NativeArray<WindowsMRManipulationGestureEvent> manipulationGestureEvents { get { return m_WindowsMRProvider.manipulationGestureEvents; } }

        /// <summary>
        /// A collection of all WindowsMRNavigationGestureEvent managed by this subsystem.
        /// This is cleared every frame and refreshed with new gesture events.
        /// </summary>
        public NativeArray<WindowsMRNavigationGestureEvent> navigationGestureEvents { get { return m_WindowsMRProvider.navigationGestureEvents; } }

        /// <summary>
        /// A collection of all WindowsMRTappedGestureEvent managed by this subsystem.
        /// This is cleared every frame and refreshed with new gesture events.
        /// </summary>
        public NativeArray<WindowsMRTappedGestureEvent> tappedGestureEvents { get { return m_WindowsMRProvider.tappedGestureEvents; } }

        /// <summary>
        /// Creates the provider interface.
        /// </summary>
        /// <returns>The provider interface for WindowsMR</returns>
        protected override Provider CreateProvider()
        {
            m_WindowsMRProvider = new WindowsMRGestureProvider(this);
            return m_WindowsMRProvider;
        }

        /// <summary>
        /// Enable the manipulation gesture.
        /// Enabling this gesture will disable the navigation gesture if it has been enabled.
        /// These gestures cannot be enabled simultaneously.
        /// </summary>
        /// <param name="enable">true if manipulation gesture should be enabled, else false.</param>
        /// <returns>true if the gesture was enabled, else false</returns>
        public bool SetEnableManipulationGesture(bool enable)
        {
            return m_WindowsMRProvider.SetEnableManipulationGesture(enable);
        }

        /// <summary>
        /// Enable the navigation gesture.
        /// Enabling this gesture will disable the manipulation gesture if it has been enabled.
        /// These gestures cannot be enabled simultaneously.
        /// </summary>
        /// <param name="enable">true if navigation gesture should be enabled, else false.</param>
        /// <returns>true if the gesture was enabled, else false</returns>
        public bool SetEnableNavigationGesture(bool enable)
        {
            return m_WindowsMRProvider.SetEnableNavigationGesture(enable);
        }

        class WindowsMRGestureProvider : Provider
        {
            WindowsMRGestureSubsystem m_Subsystem;

            public WindowsMRGestureProvider(WindowsMRGestureSubsystem subsystem)
            {
                m_Subsystem = subsystem;
            }

            public override void Start()
            {
                NativeApi.UnityWindowsMR_GesturesStart();
            }

            public override void Stop()
            {
                NativeApi.UnityWindowsMR_GesturesStop();
            }

            public override void Update()
            {
                NativeApi.UnityWindowsMR_GesturesUpdate();

                RetrieveAllGestureEvents();
            }

            unsafe delegate void* GetGestureEventsPtrFunction(out int gesturesLength, out int elementSize);

            unsafe void RetrieveGestureEvents<T>(
                GetGestureEventsPtrFunction getGestureEventsPtrFunction, ref NativeArray<T> gestureEvents) where T : struct
            {
                // gestureEventsPtr is not owned by this code, contents must be copied out in this function.
                int gestureEventsLength, elementSize;
                void* gestureEventsPtr = getGestureEventsPtrFunction(out gestureEventsLength, out elementSize);
                if (gestureEventsPtr == null || gestureEventsLength == 0)
                {
                    // Avoid re-allocation of NativeArray if our current NativeArray is empty and we don't have new events.
                    if (gestureEvents.Length > 0)
                    {
                        if (gestureEvents.IsCreated)
                            gestureEvents.Dispose();
                        gestureEvents = new NativeArray<T>(0, Allocator.Persistent);
                    }
                }
                else
                {
                    // New events, memcopy into NativeArray.
                    if (gestureEvents.IsCreated)
                        gestureEvents.Dispose();
                    gestureEvents = new NativeArray<T>(gestureEventsLength, Allocator.Persistent);

                    var sizeOfGestureEvent = UnsafeUtility.SizeOf<T>();
                    UnsafeUtility.MemCpy(gestureEvents.GetUnsafePtr(), gestureEventsPtr, elementSize * gestureEventsLength);
                }
            }

            unsafe void RetrieveAllGestureEvents()
            {
                RetrieveGestureEvents(NativeApi.UnityWindowsMR_GesturesGetHoldGestureEventsPtr, ref m_HoldGestureEvents);
                RetrieveGestureEvents(NativeApi.UnityWindowsMR_GesturesGetManipulationGestureEventsPtr, ref m_ManipulationGestureEvents);
                RetrieveGestureEvents(NativeApi.UnityWindowsMR_GesturesGetNavigationGestureEventsPtr, ref m_NavigationGestureEvents);
                RetrieveGestureEvents(NativeApi.UnityWindowsMR_GesturesGetTappedGestureEventsPtr, ref m_TappedGestureEvents);

                // Avoid re-allocation of NativeArray if our current NativeArray is empty and we don't have new events.
                if (m_TappedGestureEvents.Length == 0)
                {
                    if (m_ActivateGestureEvents.Length > 0)
                    {
                        if (m_ActivateGestureEvents.IsCreated)
                            m_ActivateGestureEvents.Dispose();
                        m_ActivateGestureEvents = new NativeArray<ActivateGestureEvent>(0, Allocator.Persistent);
                    }
                }
                else
                {
                    if (m_ActivateGestureEvents.IsCreated)
                        m_ActivateGestureEvents.Dispose();
                    m_ActivateGestureEvents = new NativeArray<ActivateGestureEvent>(m_TappedGestureEvents.Length, Allocator.Persistent);

                    int iActivateGestureEvent = 0;
                    foreach (var gestureEvent in m_TappedGestureEvents)
                    {
                        if (gestureEvent.state == GestureState.Started)
                            m_ActivateGestureEvents[iActivateGestureEvent++] =
                                new ActivateGestureEvent(GetNextGUID(), gestureEvent.state);
                    }
                }
            }

            public override void Destroy()
            {
                NativeApi.UnityWindowsMR_GesturesDestroy();

                if (m_HoldGestureEvents.IsCreated)
                    m_HoldGestureEvents.Dispose();
                if (m_ManipulationGestureEvents.IsCreated)
                    m_ManipulationGestureEvents.Dispose();
                if (m_NavigationGestureEvents.IsCreated)
                    m_NavigationGestureEvents.Dispose();
                if (m_TappedGestureEvents.IsCreated)
                    m_TappedGestureEvents.Dispose();

                base.Destroy();
            }

            public bool SetEnableManipulationGesture(bool enable)
            {
                return NativeApi.UnityWindowsMR_GesturesSetEnableManipulationGesture(enable);
            }

            public bool SetEnableNavigationGesture(bool enable)
            {
                return NativeApi.UnityWindowsMR_GesturesSetEnableNavigationGesture(enable);
            }

            public NativeArray<WindowsMRHoldGestureEvent> holdGestureEvents { get { return m_HoldGestureEvents; } }
            public NativeArray<WindowsMRManipulationGestureEvent> manipulationGestureEvents { get { return m_ManipulationGestureEvents; } }
            public NativeArray<WindowsMRNavigationGestureEvent> navigationGestureEvents { get { return m_NavigationGestureEvents; } }
            public NativeArray<WindowsMRTappedGestureEvent> tappedGestureEvents { get { return m_TappedGestureEvents; } }

            NativeArray<WindowsMRHoldGestureEvent> m_HoldGestureEvents = new NativeArray<WindowsMRHoldGestureEvent>(0, Allocator.Persistent);
            NativeArray<WindowsMRManipulationGestureEvent> m_ManipulationGestureEvents = new NativeArray<WindowsMRManipulationGestureEvent>(0, Allocator.Persistent);
            NativeArray<WindowsMRNavigationGestureEvent> m_NavigationGestureEvents = new NativeArray<WindowsMRNavigationGestureEvent>(0, Allocator.Persistent);
            NativeArray<WindowsMRTappedGestureEvent> m_TappedGestureEvents = new NativeArray<WindowsMRTappedGestureEvent>(0, Allocator.Persistent);
        }

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.SubsystemRegistration)]
        static void RegisterDescriptor()
        {
            XRGestureSubsystemDescriptor.RegisterDescriptor(
                new XRGestureSubsystemDescriptor.Cinfo
                {
                    id = "Windows Mixed Reality Gesture",
                    subsystemImplementationType = typeof(WindowsMRGestureSubsystem)
                }
            );
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
            public static extern void UnityWindowsMR_GesturesUpdate();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_GesturesStart();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_GesturesDestroy();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern void UnityWindowsMR_GesturesStop();

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe void* UnityWindowsMR_GesturesGetHoldGestureEventsPtr(out int gesturesLength, out int elementSize);

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe void* UnityWindowsMR_GesturesGetManipulationGestureEventsPtr(out int gesturesLength, out int elementSize);

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe void* UnityWindowsMR_GesturesGetNavigationGestureEventsPtr(out int gesturesLength, out int elementSize);

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe void* UnityWindowsMR_GesturesGetTappedGestureEventsPtr(out int gesturesLength, out int elementSize);

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe bool UnityWindowsMR_GesturesSetEnableManipulationGesture(bool enable);

#if UNITY_EDITOR
            [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
            [DllImport("WindowsMRXRSDK.dll")]
#else
            [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
            public static extern unsafe bool UnityWindowsMR_GesturesSetEnableNavigationGesture(bool enable);
        }

        // High GUID bits saved for common (Activate) gesture for this subsystem
        static GestureId s_NextGUID = new GestureId(1, 0);
        static GestureId GetNextGUID()
        {
            unchecked
            {
                s_NextGUID.subId1 += 1;
                if (s_NextGUID.subId1 != 0) return s_NextGUID;
                s_NextGUID.subId1 += 1;
            }

            return s_NextGUID;
        }

        WindowsMRGestureProvider m_WindowsMRProvider;
    }
}
