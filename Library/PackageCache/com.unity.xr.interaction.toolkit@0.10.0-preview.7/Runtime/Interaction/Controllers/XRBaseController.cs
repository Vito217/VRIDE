#if LIH_PRESENT
using UnityEngine.SpatialTracking;
#endif

namespace UnityEngine.XR.Interaction.Toolkit
{
    [DefaultExecutionOrder(XRInteractionUpdateOrder.k_Controllers)]
    [DisallowMultipleComponent]
    public abstract class XRBaseController : MonoBehaviour
    {
        /// <summary>
        /// The time within the frame that the controller will sample input.
        /// </summary>
        /// <seealso cref="updateTrackingType"/>
        public enum UpdateType
        {
            /// <summary>
            /// Sample input at both update, and directly before rendering. For smooth head pose tracking,
            /// we recommend using this value as it will provide the lowest input latency for the device.
            /// This is the default value for the UpdateType option
            /// </summary>
            UpdateAndBeforeRender,

            /// <summary>
            /// Only sample input during the update phase of the frame.
            /// </summary>
            Update,

            /// <summary>
            /// Only sample input directly before rendering.
            /// </summary>
            BeforeRender,
        }

        [Header("Tracking")]
        [SerializeField]
        [Tooltip("The time within the frame that the controller will sample input.")]
        UpdateType m_UpdateTrackingType = UpdateType.UpdateAndBeforeRender;

        /// <summary>
        /// The time within the frame that the controller will sample input.
        /// </summary>
        /// <seealso cref="UpdateType"/>
        public UpdateType updateTrackingType
        {
            get => m_UpdateTrackingType;
            set => m_UpdateTrackingType = value;
        }

        [SerializeField]
        [Tooltip("Whether input tracking is enabled for this controller.")]
        bool m_EnableInputTracking = true;

        /// <summary>
        /// Whether input tracking is enabled for this controller.
        /// </summary>
        public bool enableInputTracking
        {
            get => m_EnableInputTracking;
            set => m_EnableInputTracking = value;
        }

        [Header("Input")]
        [SerializeField]
        [Tooltip("Used to disable an input state changing in the interactor. Useful for swapping to a different interactor on the same object.")]
        bool m_EnableInputActions = true;

        /// <summary>
        /// Used to disable an input state changing in the interactor. Useful for swapping to a different interactor on the same object.
        /// </summary>
        public bool enableInputActions
        {
            get => m_EnableInputActions;
            set => m_EnableInputActions = value;
        }

        [Header("Model")]
        [SerializeField]
        [Tooltip("The model prefab to show for this controller.")]
        Transform m_ModelPrefab;

        /// <summary>The model prefab to show for this controller.</summary>
        public Transform modelPrefab
        {
            get => m_ModelPrefab;
            set => m_ModelPrefab = value;
        }

        [SerializeField]
        [Tooltip("The model transform that is used as the parent for the controller model.")]
        Transform m_ModelTransform;

        /// <summary>
        /// The model transform that is used as the parent for the controller model.
        /// </summary>
        /// <remarks>
        /// Automatically instantiated and set in <see cref="Awake"/>.
        /// Setting this will not automatically destroy the previous object.
        /// </remarks>
        public Transform modelTransform
        {
            get => m_ModelTransform;
            set
            {
                m_ModelTransform = value;

                // Reparent the instantiated model, see SetupModel.
                if (m_ModelGO != null)
                {
                    m_ModelGO.transform.parent = m_ModelTransform;
                }
            }
        }

        [SerializeField]
        [Tooltip("Whether this model animates in response to interaction events.")]
        bool m_AnimateModel;

        /// <summary>
        /// Whether this model animates in response to interaction events.
        /// </summary>
        public bool animateModel
        {
            get => m_AnimateModel;
            set => m_AnimateModel = value;
        }

        [SerializeField]
        [Tooltip("The animation transition to enable when selecting.")]
        string m_ModelSelectTransition;

        /// <summary>
        /// The animation transition to enable when selecting.
        /// </summary>
        public string modelSelectTransition
        {
            get => m_ModelSelectTransition;
            set => m_ModelSelectTransition = value;
        }

        [SerializeField]
        [Tooltip("The animation transition to enable when de-selecting.")]
        string m_ModelDeSelectTransition;

        /// <summary>
        /// The animation transition to enable when de-selecting.
        /// </summary>
        public string modelDeSelectTransition
        {
            get => m_ModelDeSelectTransition;
            set => m_ModelDeSelectTransition = value;
        }

        [SerializeField]
        float m_AnchorControlDeadzone = 0.75f;

        public float anchorControlDeadzone
        {
            get => m_AnchorControlDeadzone;
            set => m_AnchorControlDeadzone = value;
        }

        [SerializeField]
        float m_AnchorControlOffAxisDeadzone = 0.3f;

        public float anchorControlOffAxisDeadzone
        {
            get => m_AnchorControlOffAxisDeadzone;
            set => m_AnchorControlOffAxisDeadzone = value;
        }

        InteractionState m_SelectInteractionState;
        /// <summary>
        /// (Read Only) The current select interaction state.
        /// </summary>
        public InteractionState selectInteractionState => m_SelectInteractionState;

        InteractionState m_ActivateInteractionState;
        /// <summary>
        /// (Read Only) The current activate interaction state.
        /// </summary>
        public InteractionState activateInteractionState => m_ActivateInteractionState;

        InteractionState m_UIPressInteractionState;
        /// <summary>
        /// (Read Only) The current ui press interaction state.
        /// </summary>
        public InteractionState uiPressInteractionState => m_UIPressInteractionState;

        bool m_HideControllerModel;

        /// <summary>
        /// Whether the controller model should be hidden.
        /// </summary>
        public bool hideControllerModel
        {
            get => m_HideControllerModel;
            set
            {
                m_HideControllerModel = value;
                if (m_ModelGO != null)
                    m_ModelGO.SetActive(!m_HideControllerModel);
            }
        }

        XRControllerState m_ControllerState = new XRControllerState();

        /// <summary>
        /// The instantiated <see cref="modelPrefab"/>.
        /// </summary>
        GameObject m_ModelGO;

        /// <summary>
        /// A boolean value that indicates setup should be (re)performed on Update.
        /// </summary>
        bool m_PerformSetup = true;

        protected virtual void Awake()
        {
            // Create empty model transform if none specified
            if (m_ModelTransform == null)
            {
                var modelGO = new GameObject($"[{gameObject.name}] Model");
                m_ModelTransform = modelGO.transform;
                m_ModelTransform.SetParent(transform);
                m_ModelTransform.localPosition = Vector3.zero;
                m_ModelTransform.localRotation = Quaternion.identity;
            }
        }

        protected virtual void OnEnable()
        {
            Application.onBeforeRender += OnBeforeRender;
        }

        protected virtual void OnDisable()
        {
            Application.onBeforeRender -= OnBeforeRender;
        }

        protected void Update()
        {
            UpdateController();
        }

        void PerformSetup()
        {
            SetupModel();
        }

        void SetupModel()
        {
            if (m_ModelGO != null)
                Destroy(m_ModelGO);

            if (m_ModelPrefab != null)
            {
                m_ModelGO = Instantiate(m_ModelPrefab).gameObject;
                m_ModelGO.transform.parent = m_ModelTransform;
                m_ModelGO.transform.localPosition = Vector3.zero;
                m_ModelGO.transform.localRotation = Quaternion.identity;
                m_ModelGO.transform.localScale = Vector3.one;
                m_ModelGO.transform.gameObject.SetActive(true);
            }
        }

        protected virtual void UpdateController()
        {
            if (m_PerformSetup)
            {
                PerformSetup();
                m_PerformSetup = false;
            }

            if (m_EnableInputTracking &&
                (m_UpdateTrackingType == UpdateType.Update ||
                    m_UpdateTrackingType == UpdateType.UpdateAndBeforeRender))
            {
                UpdateTrackingInput(m_ControllerState);
            }

            if (m_EnableInputActions)
            {
                UpdateInputInternal(m_ControllerState);
            }

            // We expect that recorded input will update the controller state as needed.
            ApplyControllerState(XRInteractionUpdateOrder.UpdatePhase.Dynamic, m_ControllerState);
        }

        /// <summary>
        /// This method is automatically called for "Just Before Render" input updates for VR devices.
        /// </summary>
        /// <seealso cref="Application.onBeforeRender"/>
        protected virtual void OnBeforeRender()
        {
            if (m_EnableInputTracking &&
                (m_UpdateTrackingType == UpdateType.BeforeRender ||
                    m_UpdateTrackingType == UpdateType.UpdateAndBeforeRender))
            {
                UpdateTrackingInput(m_ControllerState);
                ApplyControllerState(XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender, m_ControllerState);
            }
        }

        public virtual bool GetControllerState(out XRControllerState controllerState)
        {
            controllerState = m_ControllerState;
            return false;
        }

        public virtual void SetControllerState(XRControllerState controllerState)
        {
            m_ControllerState = controllerState;
        }

        protected virtual void ApplyControllerState(XRInteractionUpdateOrder.UpdatePhase updatePhase, XRControllerState controllerState)
        {
            if (controllerState == null)
                return;

            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic)
            {
                // Sync the controller actions from the interaction state in the controller
                m_SelectInteractionState = controllerState.selectInteractionState;
                m_ActivateInteractionState = controllerState.activateInteractionState;
                m_UIPressInteractionState = controllerState.uiPressInteractionState;
            }

            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic ||
                updatePhase == XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender)
            {
                if ((controllerState.poseDataFlags & PoseDataFlags.Position) != 0)
                {
                    transform.localPosition = controllerState.position;
                }

                if ((controllerState.poseDataFlags & PoseDataFlags.Rotation) != 0)
                {
                    transform.localRotation = controllerState.rotation;
                }
            }
        }

        protected virtual void UpdateTrackingInput(XRControllerState controllerState) {}

        internal void UpdateInputInternal(XRControllerState controllerState)
        {
            UpdateInput(controllerState);
            UpdateControllerModelAnimation();
        }

        protected virtual void UpdateInput(XRControllerState controllerState) {}

        /// <summary>
        /// Update the animation on the instance of the <see cref="modelPrefab"/> (if the prefab contains an <see cref="Animator"/>).
        /// </summary>
        /// <seealso cref="animateModel"/>
        /// <seealso cref="modelPrefab"/>
        protected virtual void UpdateControllerModelAnimation()
        {
            if (m_ModelGO != null && m_AnimateModel)
            {
                var animator = m_ModelGO.GetComponent<Animator>();
                if (animator != null)
                {
                    if (m_SelectInteractionState.activatedThisFrame)
                        animator.SetTrigger(modelSelectTransition);
                    else if (m_SelectInteractionState.deactivatedThisFrame)
                        animator.SetTrigger(modelDeSelectTransition);
                }
            }
        }

        /// <summary>
        /// Override the current position and rotation (used for interaction state playback).
        /// </summary>
        /// <param name="localPosition">The local position of the transform to set.</param>
        /// <param name="localRotation">The local rotation of the transform to set.</param>
        internal void UpdateControllerPose(Vector3 localPosition, Quaternion localRotation)
        {
            var thisTransform = transform;
            thisTransform.localPosition = localPosition;
            thisTransform.localRotation = localRotation;
        }

        /// <summary>
        /// Play a haptic impulse on the controller if one is available.
        /// </summary>
        /// <param name="amplitude">Amplitude (from 0.0 to 1.0) to play impulse at.</param>
        /// <param name="duration">Duration (in seconds) to play haptic impulse.</param>
        /// <returns>Returns <see langword="true"/> if successful. Returns <see langword="false"/> otherwise.</returns>
        public virtual bool SendHapticImpulse(float amplitude, float duration) => false;
    }
}
