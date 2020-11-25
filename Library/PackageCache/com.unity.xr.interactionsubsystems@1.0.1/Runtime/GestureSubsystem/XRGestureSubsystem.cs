using System;
using Unity.Collections;

namespace UnityEngine.XR.InteractionSubsystems
{
    /// <summary>
    /// This class controls the lifecycle of an XR Gesture subsystem.
    /// </summary>
    public abstract class XRGestureSubsystem : Subsystem<XRGestureSubsystemDescriptor>
    {
        /// <summary>
        /// Whether the Gesture subsystem is currently running.
        /// </summary>
        public sealed override bool running { get { return m_Running; } }

        /// <summary>
        /// A collection of all <see cref="ActivateGestureEvents"/> managed by this subsystem.
        /// This is cleared every frame and refreshed with new gesture events.
        /// </summary>
        public NativeArray<ActivateGestureEvent> activateGestureEvents { get { return m_Provider.activateGestureEvents; } }

        /// <summary>
        /// Implementing classes must set this value to reflect the running state of the subsystem
        /// </summary>
        bool m_Running;

        /// <summary>
        /// Do not call this directly. Call create on a valid <see cref="XRGestureSubsystemDescriptor"/> instead.
        /// </summary>
        public XRGestureSubsystem()
        {
            m_Provider = CreateProvider();
        }

        /// <summary>
        /// Starts or resumes the Gesture subsystem.
        /// </summary>
        public sealed override void Start()
        {
            if (!m_Running)
                m_Provider.Start();

            m_Running = true;
        }

        /// <summary>
        /// Pauses the Gesture subsystem.
        /// </summary>
        public sealed override void Stop()
        {
            if (m_Running)
                m_Provider.Stop();

            m_Running = false;
        }

        /// <summary>
        /// Destroys the Gesture subsystem.
        /// </summary>
#if UNITY_2019_3_OR_NEWER
        protected sealed override void OnDestroy()
#else
        public sealed override void Destroy()
#endif
        {
            Stop();
            m_Provider.Destroy();
        }

        /// <summary>
        /// Trigger the Gesture's update loop.
        /// </summary>
        public void Update()
        {
            m_Provider.Update();
        }

        /// <summary>
        /// Implement this to provide this class with an interface to
        /// platform specific implementations.
        /// </summary>
        /// <returns>An implementation specific provider.</returns>
        protected abstract Provider CreateProvider();

        /// <summary>
        /// The API this subsystem uses to interop with
        /// different provider implementations.
        /// </summary>
        protected abstract class Provider
        {
            /// <summary>
            /// Invoked to start or resume a Gesture subsystem.
            /// </summary>
            public abstract void Start();

            /// <summary>
            /// Invoked to stop a running Gesture subsystem.
            /// </summary>
            public abstract void Stop();

            /// <summary>
            /// Perform any per-frame update logic here.
            /// </summary>
            public abstract void Update();

            /// <summary>
            /// Stop the Gesture subsystem and destroy all associated resources.
            /// </summary>
            public virtual void Destroy()
            { 
                m_ActivateGestureEvents.Dispose();
            }

            public NativeArray<ActivateGestureEvent> activateGestureEvents { get { return m_ActivateGestureEvents; } }
            protected NativeArray<ActivateGestureEvent> m_ActivateGestureEvents = new NativeArray<ActivateGestureEvent>(0, Allocator.Persistent);
        }

        Provider m_Provider;
    }
}
