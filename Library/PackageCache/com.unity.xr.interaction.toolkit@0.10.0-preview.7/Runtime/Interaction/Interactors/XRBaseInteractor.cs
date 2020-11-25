using System;
using System.Collections.Generic;
using System.Diagnostics;
using UnityEngine.Events;
using UnityEngine.Serialization;

#if UNITY_EDITOR
using UnityEditor.SceneManagement;
#endif

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// <see cref="UnityEvent"/> that responds to changes of hover and selection by this interactor.
    /// </summary>
    [Serializable]
    public class XRInteractorEvent : UnityEvent<XRBaseInteractable> {}

    /// <summary>
    /// Abstract base class from which all interactor behaviours derive.
    /// This class hooks into the interaction system (via <see cref="XRInteractionManager"/>) and provides base virtual methods for handling
    /// hover and selection.
    /// </summary>
    [SelectionBase]
    [DisallowMultipleComponent]
    [DefaultExecutionOrder(XRInteractionUpdateOrder.k_Interactors)]
    public abstract class XRBaseInteractor : MonoBehaviour
    {
        [SerializeField]
        [Tooltip("The Interaction Manager that this interactor will communicate with.")]
        XRInteractionManager m_InteractionManager;
        /// <summary>The <see cref="XRInteractionManager"/> that this interactor will communicate with.</summary>
        public XRInteractionManager interactionManager
        {
            get => m_InteractionManager;
            set
            {
                m_InteractionManager = value;
                RegisterWithInteractionManager();
            }
        }

        [SerializeField]
        [Tooltip("Only interactables with this layer mask will respond to this interactor.")]
        LayerMask m_InteractionLayerMask = -1;
        /// <summary>
        /// Only interactables with this layer mask will respond to this interactor.
        /// </summary>
        public LayerMask interactionLayerMask
        {
            get => m_InteractionLayerMask;
            set => m_InteractionLayerMask = value;
        }

        [SerializeField]
        [Tooltip("The attach transform that is used as an attach point for interactables.")]
        Transform m_AttachTransform;
        /// <summary>
        /// The attach transform that is used as an attach point for interactables.
        /// </summary>
        /// <remarks>
        /// Automatically instantiated and set in <see cref="Awake"/>.
        /// Setting this will not automatically destroy the previous object.
        /// </remarks>
        public Transform attachTransform
        {
            get => m_AttachTransform;
            set => m_AttachTransform = value;
        }

        [SerializeField]
        [Tooltip("The initial interactable that is selected by this interactor at startup (optional, may be None).")]
        XRBaseInteractable m_StartingSelectedInteractable;
        /// <summary>
        /// The initial interactable that is selected by this interactor at startup (optional, may be <see langword="null"/>).
        /// </summary>
        public XRBaseInteractable startingSelectedInteractable
        {
            get => m_StartingSelectedInteractable;
            set => m_StartingSelectedInteractable = value;
        }

        [SerializeField, FormerlySerializedAs("m_OnHoverEnter")]
        [Tooltip("Called when this interactor begins hovering an interactable.")]
        XRInteractorEvent m_OnHoverEntered = new XRInteractorEvent();
        /// <summary>
        /// Gets or sets the event that is called when this interactor begins hovering over an interactable.
        /// </summary>
        public XRInteractorEvent onHoverEntered
        {
            get => m_OnHoverEntered;
            set => m_OnHoverEntered = value;
        }

        [Obsolete("onHoverEnter has been deprecated. Use onHoverEntered instead. (UnityUpgradable) -> onHoverEntered")]
        public XRInteractorEvent onHoverEnter => onHoverEntered;

        [SerializeField, FormerlySerializedAs("m_OnHoverExit")]
        [Tooltip("Called when this interactor stops hovering an interactable.")]
        XRInteractorEvent m_OnHoverExited = new XRInteractorEvent();
        /// <summary>
        /// Gets or sets the event that is called when this interactor stops hovering over an interactable.
        /// </summary>
        public XRInteractorEvent onHoverExited
        {
            get => m_OnHoverExited;
            set => m_OnHoverExited = value;
        }

        [Obsolete("onHoverExit has been deprecated. Use onHoverExited instead. (UnityUpgradable) -> onHoverExited")]
        public XRInteractorEvent onHoverExit => onHoverExited;

        [SerializeField, FormerlySerializedAs("m_OnSelectEnter")]
        [Tooltip("Called when this interactor begins selecting an interactable.")]
        XRInteractorEvent m_OnSelectEntered = new XRInteractorEvent();
        /// <summary>
        /// Gets or sets the event that is called when this interactor begins selecting an interactable.
        /// </summary>
        public XRInteractorEvent onSelectEntered
        {
            get => m_OnSelectEntered;
            set => m_OnSelectEntered = value;
        }

        [Obsolete("onSelectEnter has been deprecated. Use onSelectEntered instead. (UnityUpgradable) -> onSelectEntered")]
        public XRInteractorEvent onSelectEnter => onSelectEntered;

        [SerializeField, FormerlySerializedAs("m_OnSelectExit")]
        [Tooltip("Called when this interactor stops selecting an interactable.")]
        XRInteractorEvent m_OnSelectExited = new XRInteractorEvent();
        /// <summary>
        /// Gets or sets the event that is called when this interactor stops selecting an interactable.
        /// </summary>
        public XRInteractorEvent onSelectExited
        {
            get => m_OnSelectExited;
            set => m_OnSelectExited = value;
        }

        [Obsolete("onSelectExit has been deprecated. Use onSelectExited instead. (UnityUpgradable) -> onSelectExited")]
        public XRInteractorEvent onSelectExit => onSelectExited;

        bool m_AllowHover = true;
        public bool allowHover
        {
            get => m_AllowHover;
            set => m_AllowHover = value;
        }

        bool m_AllowSelect = true;
        public bool allowSelect
        {
            get => m_AllowSelect;
            set => m_AllowSelect = value;
        }

        bool m_EnableInteractions;
        public bool enableInteractions
        {
            get => m_EnableInteractions;
            set
            {
                m_EnableInteractions = value;
                EnableInteractions(value);
            }
        }

        bool m_IsPerformingManualInteraction;
        public bool isPerformingManualInteraction => m_IsPerformingManualInteraction;

        /// <summary>
        /// Selected interactable for this interactor (may be <see langword="null"/>).
        /// </summary>
        public XRBaseInteractable selectTarget { get; protected set; }

        /// <summary>
        /// Target interactables that are currently being hovered over (may by empty).
        /// </summary>
        protected List<XRBaseInteractable> hoverTargets { get; } = new List<XRBaseInteractable>();

        bool m_RequiresRegistration = true;

        XRInteractionManager m_RegisteredInteractionManager;

        /// <summary>
        /// Cached reference to an <see cref="XRInteractionManager"/> found with <see cref="Object.FindObjectOfType"/>.
        /// </summary>
        static XRInteractionManager s_InteractionManagerCache;

        [Conditional("UNITY_EDITOR")]
        protected virtual void Reset()
        {
            FindInteractionManagerEditTime();
        }

        protected virtual void Awake()
        {
            // Create empty attach transform if none specified
            if (m_AttachTransform == null)
            {
                var attachGO = new GameObject($"[{gameObject.name}] Attach");
                m_AttachTransform = attachGO.transform;
                m_AttachTransform.SetParent(transform);
                m_AttachTransform.localPosition = Vector3.zero;
                m_AttachTransform.localRotation = Quaternion.identity;
            }

            // Setup interaction manager
            FindCreateInteractionManager();
            RegisterWithInteractionManager();
        }

        protected virtual void OnEnable()
        {
            if (m_RequiresRegistration)
            {
                FindCreateInteractionManager();
                if (m_RegisteredInteractionManager != null)
                    m_RegisteredInteractionManager.RegisterInteractor(this);

                m_RequiresRegistration = false;
            }

            EnableInteractions(true);
        }

        protected virtual void OnDisable()
        {
            if (m_RegisteredInteractionManager != null)
                m_RegisteredInteractionManager.UnregisterInteractor(this);

            m_RequiresRegistration = true;
        }

        protected virtual void Start()
        {
            if (m_InteractionManager != null && m_StartingSelectedInteractable != null)
                m_InteractionManager.ForceSelect(this, m_StartingSelectedInteractable);
        }

        protected virtual void OnDestroy()
        {
            if (m_RegisteredInteractionManager != null)
                m_RegisteredInteractionManager.UnregisterInteractor(this);
        }

        /// <summary>
        /// Retrieves a copy of the list of target interactables that are currently being hovered over.
        /// </summary>
        /// <param name="targets">The list to store hover targets into.</param>
        /// <remarks>
        /// Clears <paramref name="targets"/> before adding to it.
        /// </remarks>
        public void GetHoverTargets(List<XRBaseInteractable> targets)
        {
            targets.Clear();
            foreach (var target in hoverTargets)
            {
                targets.Add(target);
            }
        }

        [Conditional("UNITY_EDITOR")]
        void FindInteractionManagerEditTime()
        {
#if UNITY_EDITOR
            // Find the interaction manager in the same scene
            // (since serialization of cross scene references are not supported).
            var currentStage = StageUtility.GetCurrentStageHandle();
            var interactionManagers = currentStage.FindComponentsOfType<XRInteractionManager>();
            foreach (var manager in interactionManagers)
            {
                if (manager.gameObject.scene == gameObject.scene)
                {
                    m_InteractionManager = manager;
                    break;
                }
            }
#endif
        }

        void FindCreateInteractionManager()
        {
            if (m_InteractionManager != null)
                return;

            if (s_InteractionManagerCache == null)
                s_InteractionManagerCache = FindObjectOfType<XRInteractionManager>();

            if (s_InteractionManagerCache == null)
            {
                var interactionManagerGO = new GameObject("XR Interaction Manager", typeof(XRInteractionManager));
                s_InteractionManagerCache = interactionManagerGO.GetComponent<XRInteractionManager>();
            }

            m_InteractionManager = s_InteractionManagerCache;
        }

        void RegisterWithInteractionManager()
        {
            if (m_RegisteredInteractionManager != m_InteractionManager)
            {
                if (m_RegisteredInteractionManager != null)
                {
                    m_RegisteredInteractionManager.UnregisterInteractor(this);
                    m_RegisteredInteractionManager = null;
                }

                if (m_InteractionManager != null)
                {
                    m_InteractionManager.RegisterInteractor(this);
                    m_RegisteredInteractionManager = m_InteractionManager;
                }
            }

            if (m_RegisteredInteractionManager != null)
                m_RequiresRegistration = false;
        }

        bool IsOnValidLayerMask(XRBaseInteractable interactable)
        {
            return (interactionLayerMask & interactable.interactionLayerMask) != 0;
        }

        void EnableInteractions(bool enable)
        {
            m_AllowHover = enable;
            m_AllowSelect = enable;
        }

        /// <summary>
        /// Retrieve the list of interactables that this interactor could possibly interact with this frame.
        /// </summary>
        /// <param name="validTargets">Populated List of interactables that are valid for selection or hover.</param>
        public abstract void GetValidTargets(List<XRBaseInteractable> validTargets);

        /// <summary>
        /// (Read Only) Indicates whether this interactor is in a state where it could hover.
        /// </summary>
        public virtual bool isHoverActive => m_AllowHover;

        /// <summary>
        /// (Read Only) Indicates whether this interactor is in a state where it could select.
        /// </summary>
        public virtual bool isSelectActive => m_AllowSelect;

        /// <summary>
        /// Determines if the interactable is valid for hover this frame.
        /// </summary>
        /// <param name="interactable">Interactable to check.</param>
        /// <returns>Returns <see langword="true"/> if the interactable can be hovered over this frame.</returns>
        public virtual bool CanHover(XRBaseInteractable interactable) => m_AllowHover && IsOnValidLayerMask(interactable);

        /// <summary>
        /// Determines if the interactable is valid for selection this frame.
        /// </summary>
        /// <param name="interactable">Interactable to check.</param>
        /// <returns>Returns <see langword="true"/> if the interactable can be selected this frame.</returns>
        public virtual bool CanSelect(XRBaseInteractable interactable) => m_AllowSelect && IsOnValidLayerMask(interactable);

        /// <summary>
        /// (Read Only) Indicates whether this interactor requires exclusive selection of an interactable.
        /// </summary>
        /// <remarks>
        /// That is, this interactor will only be selected if exactly one interactor is trying to select it.
        /// </remarks>
        public virtual bool requireSelectExclusive => false;

        /// <summary>
        /// (Read Only) Overriding movement type of the selected interactable's movement.
        /// </summary>
        public virtual XRBaseInteractable.MovementType? selectedInteractableMovementTypeOverride => null;

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor first initiates hovering over an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is being hovered over.</param>
        /// <seealso cref="OnHoverEntered"/>
        protected internal virtual void OnHoverEntering(XRBaseInteractable interactable)
        {
            hoverTargets.Add(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor first initiates hovering over an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is being hovered over.</param>
        /// <seealso cref="OnHoverExited"/>
        protected internal virtual void OnHoverEntered(XRBaseInteractable interactable)
        {
            m_OnHoverEntered?.Invoke(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor ends hovering over an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is no longer hovered over.</param>
        /// <seealso cref="OnHoverExited"/>
        protected internal virtual void OnHoverExiting(XRBaseInteractable interactable)
        {
            Debug.Assert(hoverTargets.Contains(interactable), this);
            hoverTargets.Remove(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor ends hovering over an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is no longer hovered over.</param>
        /// <seealso cref="OnHoverEntered"/>
        protected internal virtual void OnHoverExited(XRBaseInteractable interactable)
        {
            m_OnHoverExited?.Invoke(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the first initiates selection of an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is being selected.</param>
        /// <seealso cref="OnSelectEntered"/>
        protected internal virtual void OnSelectEntering(XRBaseInteractable interactable)
        {
            selectTarget = interactable;
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor first initiates selection of an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is being selected.</param>
        /// <seealso cref="OnSelectExited"/>
        protected internal virtual void OnSelectEntered(XRBaseInteractable interactable)
        {
            m_OnSelectEntered?.Invoke(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor ends selection of an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is no longer selected.</param>
        /// <seealso cref="OnSelectExited"/>
        protected internal virtual void OnSelectExiting(XRBaseInteractable interactable)
        {
            Debug.Assert(selectTarget == interactable, this);
            if (selectTarget == interactable)
                selectTarget = null;
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor ends selection of an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is no longer selected.</param>
        /// <seealso cref="OnSelectEntered"/>
        protected internal virtual void OnSelectExited(XRBaseInteractable interactable)
        {
            m_OnSelectExited?.Invoke(interactable);
        }

        /// <summary>
        /// This method is called by the interaction manager to update the interactor.
        /// Please see the interaction manager documentation for more details on update order.
        /// </summary>

        public virtual void ProcessInteractor(XRInteractionUpdateOrder.UpdatePhase updatePhase) {}

        /// <summary>
        /// Manually initiate selection of an interactable.
        /// </summary>
        /// <param name="interactable">Interactable that is being selected.</param>
        /// <seealso cref="EndManualInteraction"/>
        public virtual void StartManualInteraction(XRBaseInteractable interactable)
        {
            OnSelectEntering(interactable);
            OnSelectEntered(interactable);
            m_IsPerformingManualInteraction = true;
        }

        /// <summary>
        /// Ends the manually initiated selection of an interactable.
        /// </summary>
        /// <seealso cref="StartManualInteraction"/>
        public virtual void EndManualInteraction()
        {
            if (m_IsPerformingManualInteraction)
            {
                OnSelectExiting(selectTarget);
                OnSelectExited(selectTarget);
                m_IsPerformingManualInteraction = false;
            }
        }
    }
}
