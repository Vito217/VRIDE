# About Interaction Subsystems

The purpose of this `com.unity.xr.interactionsubsystems` package is to provide definitions of all subsystems that enable XR interaction functionality.

Presently, this package defines the following XR subsystems:

# GestureSubsystem

The GestureSubsystem provides access to both platform-specific as well as common gestures through a polling mechanism.  On it's own, the base XRGestureSubsystem class does not get access to gestures.  It must be implemented by a platform-specific gesture subsystem that polls for gestures from a native provider each frame.  These gestures are then added to specific arrays of both platform-specific and common gestures each frame.

## Common Gestures

### Activate

The activate gesture indicates that the user wants to interact with some object in the world.  This can be triggered by tapping in the air, placing a finger up or tapping a touch-screen.  It is up to the platform to define what source gesture should cause this (it should be the appropriate one for a given platform and consistent with how a user otherwise interacts with UI and world objects).  Typically this will activate whatever object is centered in the users view.

See the [ActivateGestureEvent](/Runtime/GestureSubsystem/Gestures/ActivateGestureEvent.cs) for more details.

## Example Platform Implementation

The following pseudo-code is an implementation of a derived platform-specific implementation that polls a native platform (via PInvoke into DLL exported functions) for gestures.  It stores and exposes the platform-specific gestures in a `platformGestureEvents` NativeArray that can be polled every frame.  It also translates these gestures into the common Activate gesture (it is up to the platform on how this translation should happen, but the source gesture should map to the common gesture in a logical way).

```csharp
public sealed class PlatfromGestureSubsystem : XRGestureSubsystem
{
    /// <summary>
    /// A collection of platform specific gestures
    /// This is cleared every frame and refreshed with new gesture events.
    /// </summary>
    public NativeArray<PlatformGestureEvent> platformGestureEvents { get { return m_PlatformProvider.platformGestureEvents; } }

    /// <summary>
    /// Creates the provider interface.
    /// </summary>
    /// <returns>The provider interface for Platform</returns>
    protected override Provider CreateProvider()
    {
        m_PlatformProvider = new PlatformGestureProvider(this);
        return m_PlatformProvider;
    }

    class PlatformGestureProvider : Provider
    {
        PlatformGestureSubsystem m_Subsystem;

        public PlatformGestureProvider(PlatformGestureSubsystem subsystem)
        {
            m_Subsystem = subsystem;
        }

        public override void Start()
        {
            NativeApi.UnityPlatform_GesturesStart();
        }

        public override void Stop()
        {
            NativeApi.UnityPlatform_GesturesStop();
        }

        public override void Update()
        {
            NativeApi.UnityPlatform_GesturesUpdate();

            RetrieveAllGestureEvents();
        }

        unsafe delegate void* GetGestureEventsPtrFunction(out int gesturesLength, out int elementSize);

        unsafe void RetrieveAllGestureEvents()
        {
            // Poll native code for platform gesture events here and place into NativeArray
            int gestureEventsLength, elementSize;
            void* gestureEventsPtr = NativeApi.UnityPlatform_GesturesGetGestureEventsPtr(out gestureEventsLength, out elementSize);
            if (gestureEventsPtr == null)
                return;
            if (m_PlatformGestureEvents.IsCreated)
                m_PlatformGestureEvents.Dispose();
            m_PlatformGestureEvents = new NativeArray<PlatformKeyPoseGestureEvent>(gestureEventsLength, Allocator.Persistent);
            var sizeOfPlatformKeyPoseGestureEvent = UnsafeUtility.SizeOf<PlatformKeyPoseGestureEvent>();
            UnsafeUtility.MemCpy(m_PlatformGestureEvents.GetUnsafePtr(), gestureEventsPtr, elementSize * gestureEventsLength);

            // Translate platform gestures into common Activate gestures
            if (m_ActivateGestureEvents.IsCreated)
                m_ActivateGestureEvents.Dispose();
            m_ActivateGestureEvents = new NativeArray<ActivateGestureEvent>(activateGestureEventCount, Allocator.Persistent);
            for (var i = 0; i < m_PlatformGestureEvents.Length, ++i)
            {
                // Translate every gesture here but we typically want to filter on specific gestures
                m_ActivateGestureEvents[i] = new ActivateGestureEvent(GetNextGUID(), gestureEvent.state);
            }
        }

        public override void Destroy()
        {
            NativeApi.UnityPlatform_GesturesDestroy();

            m_PlatformGestureEvents.Dispose();

            base.Destroy();
        }

        public NativeArray<PlatformGestureEvent> platformGestureEvents { get { return m_PlatformGestureEvents; } }
        NativeArray<PlatformGestureEvent> m_PlatformGestureEvents = new NativeArray<PlatformGestureEvent>(0, Allocator.Persistent);
    }

#if UNITY_EDITOR || PLATFORM_LUMIN
    [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
#endif
    static void RegisterDescriptor()
    {
        XRGestureSubsystemDescriptor.RegisterDescriptor(
            new XRGestureSubsystemDescriptor.Cinfo
            {
                id = "Platform Gesture",
                subsystemImplementationType = typeof(PlatformGestureSubsystem)
            }
        );
    }

    static class NativeApi
    {
        // Containes DLLImported native provider function declarations
        [DllImport("Platform.dll", CharSet=CharSet.Auto)]
        public static extern void UnityPlatform_GesturesUpdate();

        [DllImport("Platform.dll", CharSet=CharSet.Auto)]
        public static extern void UnityPlatform_GesturesStart();

        [DllImport("Platform.dll", CharSet=CharSet.Auto)]
        public static extern void UnityPlatform_GesturesDestroy();

        [DllImport("Platform.dll", CharSet=CharSet.Auto)]
        public static extern void UnityPlatform_GesturesStop();

        [DllImport("Platform.dll", CharSet=CharSet.Auto)]
        public static extern unsafe void* UnityPlatform_GesturesGetPlatformGestureEventsPtr(out int gesturesLength, out int elementSize);
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

    PlatformGestureProvider m_PlatformProvider;
}
```
