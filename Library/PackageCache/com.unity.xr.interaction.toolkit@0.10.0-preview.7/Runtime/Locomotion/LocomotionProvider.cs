using System;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The <see cref="LocomotionProvider"/> is the base class for various locomotion implementations.
    /// This class provides simple ways to interrogate the <see cref="LocomotionSystem"/> for whether a locomotion can begin
    /// and simple events for hooking into a start/end locomotion.
    /// </summary>
    public abstract class LocomotionProvider : MonoBehaviour
    {
        /// <summary>
        /// The <see cref="startLocomotion"/> action will be called when a <see cref="LocomotionProvider"/> successfully begins a locomotion event.
        /// </summary>
        public event Action<LocomotionSystem> startLocomotion;

        /// <summary>
        /// The <see cref="endLocomotion"/> action will be called when a <see cref="LocomotionProvider"/> successfully ends a locomotion event.
        /// </summary>
        public event Action<LocomotionSystem> endLocomotion;

        [SerializeField]
        [Tooltip("The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig." +
            " If one is not provided, the behavior will attempt to locate one during its Awake call.")]
        LocomotionSystem m_System;

        /// <summary>
        /// The <see cref="LocomotionSystem"/> that this <see cref="LocomotionProvider"/> will communicate with for exclusive access to an XR Rig.
        /// If one is not provided, the behavior will attempt to locate one during its Awake call.
        /// </summary>
        public LocomotionSystem system
        {
            get => m_System;
            set => m_System = value;
        }

        protected virtual void Awake()
        {
            if (m_System == null)
                m_System = FindObjectOfType<LocomotionSystem>();
        }

        protected bool CanBeginLocomotion()
        {
            if (m_System == null)
                return false;

            return !m_System.busy;
        }

        protected bool BeginLocomotion()
        {
            if (m_System == null)
                return false;

            var success = m_System.RequestExclusiveOperation(this) == RequestResult.Success;
            if (success)
                startLocomotion?.Invoke(m_System);

            return success;
        }

        protected bool EndLocomotion()
        {
            if (m_System == null)
                return false;

            var success = m_System.FinishExclusiveOperation(this) == RequestResult.Success;
            if (success)
                endLocomotion?.Invoke(m_System);

            return success;
        }
    }
}
