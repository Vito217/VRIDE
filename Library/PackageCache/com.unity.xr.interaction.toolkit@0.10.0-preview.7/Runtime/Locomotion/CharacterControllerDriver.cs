namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Drives a <see cref="CharacterController"/> height
    /// upon locomotion events of a <see cref="LocomotionProvider"/>.
    /// </summary>
    public class CharacterControllerDriver : MonoBehaviour
    {
        [SerializeField]
        [Tooltip("The Locomotion Provider object to listen to.")]
        LocomotionProvider m_LocomotionProvider;
        /// <summary>
        /// The <see cref="LocomotionProvider"/> object to listen to.
        /// </summary>
        public LocomotionProvider locomotionProvider
        {
            get => m_LocomotionProvider;
            set
            {
                Unsubscribe(m_LocomotionProvider);
                m_LocomotionProvider = value;
                Subscribe(m_LocomotionProvider);

                SetupCharacterController();
                UpdateCharacterController();
            }
        }

        [SerializeField]
        [Tooltip("The minimum height of the character's capsule that will be set by this behavior.")]
        float m_MinHeight;
        /// <summary>
        /// The minimum height of the character's capsule that will be set by this behavior.
        /// </summary>
        /// <seealso cref="maxHeight"/>
        /// <seealso cref="CharacterController.height"/>
        public float minHeight
        {
            get => m_MinHeight;
            set => m_MinHeight = value;
        }

        [SerializeField]
        [Tooltip("The maximum height of the character's capsule that will be set by this behavior.")]
        float m_MaxHeight = float.PositiveInfinity;
        /// <summary>
        /// The maximum height of the character's capsule that will be set by this behavior.
        /// </summary>
        /// <seealso cref="minHeight"/>
        /// <seealso cref="CharacterController.height"/>
        public float maxHeight
        {
            get => m_MaxHeight;
            set => m_MaxHeight = value;
        }

        XRRig m_XRRig;
        /// <summary>
        /// (Read Only) The <see cref="XRRig"/> used for driving the <see cref="CharacterController"/>.
        /// </summary>
        protected XRRig xrRig => m_XRRig;

        CharacterController m_CharacterController;
        /// <summary>
        /// (Read Only) The <see cref="CharacterController"/> that this class drives.
        /// </summary>
        protected CharacterController characterController => m_CharacterController;

        protected void Awake()
        {
            if (m_LocomotionProvider == null)
                m_LocomotionProvider = GetComponent<ContinuousMoveProviderBase>();
        }

        protected void OnEnable()
        {
            Subscribe(m_LocomotionProvider);
        }

        protected void OnDisable()
        {
            Unsubscribe(m_LocomotionProvider);
        }

        protected void Start()
        {
            SetupCharacterController();
            UpdateCharacterController();
        }

        /// <summary>
        /// Update the <see cref="CharacterController.height"/> and <see cref="CharacterController.center"/>
        /// based on the camera's position.
        /// </summary>
        protected virtual void UpdateCharacterController()
        {
            if (m_XRRig == null || m_CharacterController == null)
                return;

            var height = Mathf.Clamp(m_XRRig.cameraInRigSpaceHeight, m_MinHeight, m_MaxHeight);

            Vector3 center = m_XRRig.cameraInRigSpacePos;
            center.y = height / 2f + m_CharacterController.skinWidth;

            m_CharacterController.height = height;
            m_CharacterController.center = center;
        }

        void Subscribe(LocomotionProvider provider)
        {
            if (provider != null)
            {
                provider.startLocomotion += OnStartLocomotion;
                provider.endLocomotion += OnEndLocomotion;
            }
        }

        void Unsubscribe(LocomotionProvider provider)
        {
            if (provider != null)
            {
                provider.startLocomotion -= OnStartLocomotion;
                provider.endLocomotion -= OnEndLocomotion;
            }
        }

        void SetupCharacterController()
        {
            if (m_LocomotionProvider == null || m_LocomotionProvider.system == null)
                return;

            m_XRRig = m_LocomotionProvider.system.xrRig;
#pragma warning disable IDE0031 // Use null propagation -- Do not use for UnityEngine.Object types
            m_CharacterController = m_XRRig != null ? m_XRRig.rig.GetComponent<CharacterController>() : null;
#pragma warning restore IDE0031

            if (m_CharacterController == null && m_XRRig != null)
            {
                Debug.LogError($"Could not get CharacterController on {m_XRRig.rig}, unable to drive properties." +
                    $" Ensure there is a CharacterController on the \"Rig\" GameObject of {m_XRRig}.",
                    this);
            }
        }

        void OnStartLocomotion(LocomotionSystem system)
        {
            UpdateCharacterController();
        }

        void OnEndLocomotion(LocomotionSystem system)
        {
            UpdateCharacterController();
        }
    }
}
