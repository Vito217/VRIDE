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
    /// <see cref="UnityEvent"/> that responds to changes of hover, selection, and activation by this interactable.
    /// </summary>
    [Serializable]
    public class XRInteractableEvent : UnityEvent<XRBaseInteractor> {}

    /// <summary>
    /// Abstract base class from which all interactable behaviours derive.
    /// This class hooks into the interaction system (via <see cref="XRInteractionManager"/>) and provides base virtual methods for handling
    /// hover and selection.
    /// </summary>
    [SelectionBase]
    [DefaultExecutionOrder(XRInteractionUpdateOrder.k_Interactables)]
    public abstract class XRBaseInteractable : MonoBehaviour
    {
        /// <summary>
        /// Type of movement for an interactable.
        /// </summary>
        public enum MovementType
        {
            /// <summary>
            /// In VelocityTracking mode, the Rigidbody associated with the will have velocity and angular velocity added to it such that the interactable attach point will follow the interactor attach point
            /// as this is applying forces to the Rigidbody, this will appear to be a slight distance behind the visual representation of the Interactor / Controller.
            /// </summary>
            VelocityTracking,

            /// <summary>
            /// In Kinematic mode the Rigidbody associated with the interactable will be moved such that the interactable attach point will match the interactor attach point
            /// as this is updating the Rigidbody, this will appear a frame behind the visual representation of the Interactor / Controller.
            /// </summary>
            Kinematic,

            /// <summary>
            /// In Instantaneous Mode the interactable's transform is updated such that the interactable attach point will match the interactor's attach point.
            /// as this is updating the transform directly, any rigid body attached to the GameObject that the interactable component is on will be disabled while being interacted with so
            /// that any motion will not "judder" due to the rigid body interfering with motion.
            /// </summary>
            Instantaneous,
        }

        [SerializeField]
        [Tooltip("Manager to handle all interaction management (will find one if empty).")]
        XRInteractionManager m_InteractionManager;

        /// <summary>
        /// Manager to handle all interaction management (will find one if empty).
        /// </summary>
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
        [Tooltip("Colliders to use for interaction with this interactable (if empty, will use any child colliders).")]
#pragma warning disable IDE0044 // Add readonly modifier -- readonly fields cannot be serialized by Unity
        List<Collider> m_Colliders = new List<Collider>();
#pragma warning restore IDE0044

        /// <summary>
        /// (Read Only) Colliders to use for interaction with this interactable (if empty, will use any child colliders).
        /// </summary>
        public List<Collider> colliders => m_Colliders;

        [SerializeField]
        [Tooltip("Only interactors with this Layer Mask will interact with this interactable.")]
        LayerMask m_InteractionLayerMask = -1;

        /// <summary>
        /// Only interactors with this Layer Mask will interact with this interactable.
        /// </summary>
        public LayerMask interactionLayerMask
        {
            get => m_InteractionLayerMask;
            set => m_InteractionLayerMask = value;
        }

        readonly List<XRBaseInteractor> m_HoveringInteractors = new List<XRBaseInteractor>();

        /// <summary>
        /// (Read Only) The list of interactors that are hovering on this interactable.
        /// </summary>
        public List<XRBaseInteractor> hoveringInteractors => m_HoveringInteractors;

        /// <summary>
        /// (Read Only) Indicates whether this interactable is currently being hovered.
        /// </summary>
        public bool isHovered { get; private set; }

        /// <summary>
        /// (Read Only) Indicates whether this interactable is currently being selected.
        /// </summary>
        public bool isSelected { get; private set; }

        XRInteractionManager m_RegisteredInteractionManager;

        [SerializeField, FormerlySerializedAs("m_OnFirstHoverEnter")]
        [Tooltip("Called only when the first interactor begins hovering over this interactable.")]
        XRInteractableEvent m_OnFirstHoverEntered = new XRInteractableEvent();

        /// <summary>
        /// Called only when the first interactor begins hovering over this interactable.
        /// </summary>
        public XRInteractableEvent onFirstHoverEntered
        {
            get => m_OnFirstHoverEntered;
            set => m_OnFirstHoverEntered = value;
        }

        [Obsolete("onFirstHoverEnter has been deprecated. Use onFirstHoverEntered instead. (UnityUpgradable) -> onFirstHoverEntered")]
        public XRInteractableEvent onFirstHoverEnter => onFirstHoverEntered;

        [SerializeField, FormerlySerializedAs("m_OnHoverEnter")]
        [Tooltip("Called every time when an interactor begins hovering over this interactable.")]
        XRInteractableEvent m_OnHoverEntered = new XRInteractableEvent();

        /// <summary>
        /// Called every time when an interactor begins hovering over this interactable.
        /// </summary>
        public XRInteractableEvent onHoverEntered
        {
            get => m_OnHoverEntered;
            set => m_OnHoverEntered = value;
        }

        [Obsolete("onHoverEnter has been deprecated. Use onHoverEntered instead. (UnityUpgradable) -> onHoverEntered")]
        public XRInteractableEvent onHoverEnter => onHoverEntered;

        [SerializeField, FormerlySerializedAs("m_OnHoverExit")]
        [Tooltip("Called every time when an interactor stops hovering over this interactable.")]
        XRInteractableEvent m_OnHoverExited = new XRInteractableEvent();

        /// <summary>
        /// Called every time when an interactor stops hovering over this interactable.
        /// </summary>
        public XRInteractableEvent onHoverExited
        {
            get => m_OnHoverExited;
            set => m_OnHoverExited = value;
        }

        [Obsolete("onHoverExit has been deprecated. Use onHoverExited instead. (UnityUpgradable) -> onHoverExited")]
        public XRInteractableEvent onHoverExit => onHoverExited;

        [SerializeField, FormerlySerializedAs("m_OnLastHoverExit")]
        [Tooltip("Called only when the last interactor stops hovering over this interactable.")]
        XRInteractableEvent m_OnLastHoverExited = new XRInteractableEvent();

        /// <summary>
        /// Called only when the last interactor stops hovering over this interactable.
        /// </summary>
        public XRInteractableEvent onLastHoverExited
        {
            get => m_OnLastHoverExited;
            set => m_OnLastHoverExited = value;
        }

        [Obsolete("onLastHoverExit has been deprecated. Use onLastHoverExited instead. (UnityUpgradable) -> onLastHoverExited")]
        public XRInteractableEvent onLastHoverExit => onLastHoverExited;

        [SerializeField, FormerlySerializedAs("m_OnSelectEnter")]
        [Tooltip("Called when an interactor begins selecting this interactable.")]
        XRInteractableEvent m_OnSelectEntered = new XRInteractableEvent();

        /// <summary>
        /// Called when an interactor begins selecting this interactable.
        /// </summary>
        public XRInteractableEvent onSelectEntered
        {
            get => m_OnSelectEntered;
            set => m_OnSelectEntered = value;
        }

        [Obsolete("onSelectEnter has been deprecated. Use onSelectEntered instead. (UnityUpgradable) -> onSelectEntered")]
        public XRInteractableEvent onSelectEnter => onSelectEntered;

        [SerializeField, FormerlySerializedAs("m_OnSelectExit")]
        [Tooltip("Called when an interactor stops selecting this interactable.")]
        XRInteractableEvent m_OnSelectExited = new XRInteractableEvent();

        /// <summary>
        /// Called when an interactor stops selecting this interactable.
        /// </summary>
        public XRInteractableEvent onSelectExited
        {
            get => m_OnSelectExited;
            set => m_OnSelectExited = value;
        }

        [Obsolete("onSelectExit has been deprecated. Use onSelectExited instead. (UnityUpgradable) -> onSelectExited")]
        public XRInteractableEvent onSelectExit => onSelectExited;

        [SerializeField, FormerlySerializedAs("m_OnSelectCancel")]
        [Tooltip("Called when the interactor selecting this interactable is disabled or destroyed.")]
        XRInteractableEvent m_OnSelectCanceled = new XRInteractableEvent();

        /// <summary>
        /// Called when the interactor selecting this interactable is disabled or destroyed.
        /// </summary>
        public XRInteractableEvent onSelectCanceled
        {
            get => m_OnSelectCanceled;
            set => m_OnSelectCanceled = value;
        }

        [Obsolete("onSelectCancel has been deprecated. Use onSelectCanceled instead. (UnityUpgradable) -> onSelectCanceled")]
        public XRInteractableEvent onSelectCancel => onSelectCanceled;

        [SerializeField]
        [Tooltip("Called when an Interactor activates this selected Interactable.")]
        XRInteractableEvent m_OnActivate = new XRInteractableEvent();

        /// <summary>
        /// Called when an Interactor activates this selected Interactable.
        /// </summary>
        public XRInteractableEvent onActivate
        {
            get => m_OnActivate;
            set => m_OnActivate = value;
        }

        [SerializeField]
        [Tooltip("Called when an Interactor deactivates this selected Interactable.")]
        XRInteractableEvent m_OnDeactivate = new XRInteractableEvent();

        /// <summary>
        /// Called when an Interactor deactivates this selected Interactable.
        /// </summary>
        public XRInteractableEvent onDeactivate
        {
            get => m_OnDeactivate;
            set => m_OnDeactivate = value;
        }

        [SerializeField]
        [Tooltip("The reticle that will appear at the end of the line when it is valid.")]
        GameObject m_CustomReticle;

        /// <summary>The reticle that will appear at the end of the line when it is valid.</summary>
        public GameObject customReticle
        {
            get => m_CustomReticle;
            set => m_CustomReticle = value;
        }

        readonly Dictionary<XRBaseInteractor, GameObject> m_ReticleCache = new Dictionary<XRBaseInteractor, GameObject>();

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
            // If no colliders were set, populate with children colliders
            if (m_Colliders.Count == 0)
                GetComponentsInChildren(m_Colliders);

            // Setup interaction manager
            FindCreateInteractionManager();
            RegisterWithInteractionManager();
        }

        protected virtual void OnDestroy()
        {
            if (m_RegisteredInteractionManager != null)
                m_RegisteredInteractionManager.UnregisterInteractable(this);
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
                    m_RegisteredInteractionManager.UnregisterInteractable(this);
                    m_RegisteredInteractionManager = null;
                }

                if (m_InteractionManager != null)
                {
                    m_InteractionManager.RegisterInteractable(this);
                    m_RegisteredInteractionManager = m_InteractionManager;
                }
            }
        }

        /// <summary>
        /// Calculates distance squared to interactor (based on colliders).
        /// </summary>
        /// <param name="interactor">Interactor to calculate distance against.</param>
        /// <returns>Returns the minimum distance between the interactor and this interactable's colliders.</returns>
        public float GetDistanceSqrToInteractor(XRBaseInteractor interactor)
        {
            if (interactor == null)
                return float.MaxValue;

            var minDistanceSqr = float.MaxValue;
            foreach (var col in m_Colliders)
            {
                var offset = (interactor.attachTransform.position - col.transform.position);
                minDistanceSqr = Mathf.Min(offset.sqrMagnitude, minDistanceSqr);
            }
            return minDistanceSqr;
        }

        bool IsOnValidLayerMask(XRBaseInteractor interactor)
        {
            return (interactionLayerMask & interactor.interactionLayerMask) != 0;
        }

        /// <summary>
        /// Determines if this interactable can be hovered by a given interactor.
        /// </summary>
        /// <param name="interactor">Interactor to check for a valid hover state with.</param>
        /// <returns>Returns <see langword="true"/> if hovering is valid this frame. Returns <see langword="false"/> if not.</returns>
        public virtual bool IsHoverableBy(XRBaseInteractor interactor) => IsOnValidLayerMask(interactor);

        /// <summary>
        /// Determines if this interactable can be selected by a given interactor.
        /// </summary>
        /// <param name="interactor">Interactor to check for a valid selection with.</param>
        /// <returns>Returns <see langword="true"/> if selection is valid this frame. Returns <see langword="false"/> if not.</returns>
        public virtual bool IsSelectableBy(XRBaseInteractor interactor) => IsOnValidLayerMask(interactor);

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor first initiates hovering over an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the hover.</param>
        /// <seealso cref="OnHoverEntered"/>
        protected internal virtual void OnHoverEntering(XRBaseInteractor interactor)
        {
            if (m_CustomReticle != null)
                AttachCustomReticle(interactor);

            isHovered = true;
            m_HoveringInteractors.Add(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor first initiates hovering over an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the hover.</param>
        /// <seealso cref="OnHoverExited"/>
        protected internal virtual void OnHoverEntered(XRBaseInteractor interactor)
        {
            if (m_HoveringInteractors.Count == 1)
                m_OnFirstHoverEntered?.Invoke(interactor);

            m_OnHoverEntered?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor ends hovering over an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the hover.</param>
        /// <seealso cref="OnHoverExited"/>
        protected internal virtual void OnHoverExiting(XRBaseInteractor interactor)
        {
            if (m_CustomReticle != null)
                RemoveCustomReticle(interactor);

            isHovered = false;
            m_HoveringInteractors.Remove(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor ends hovering over an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the hover.</param>
        /// <seealso cref="OnHoverEntered"/>
        protected internal virtual void OnHoverExited(XRBaseInteractor interactor)
        {
            if (m_HoveringInteractors.Count == 0)
                m_OnLastHoverExited?.Invoke(interactor);

            m_OnHoverExited?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor first initiates selection of an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the selection.</param>
        /// <seealso cref="OnSelectEntered"/>
        protected internal virtual void OnSelectEntering(XRBaseInteractor interactor)
        {
            isSelected = true;
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor first initiates selection of an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the selection.</param>
        /// <seealso cref="OnSelectExited"/>
        /// <seealso cref="OnSelectCanceled"/>
        protected internal virtual void OnSelectEntered(XRBaseInteractor interactor)
        {
            m_OnSelectEntered?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor ends selection of an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the selection.</param>
        /// <seealso cref="OnSelectExited"/>
        protected internal virtual void OnSelectExiting(XRBaseInteractor interactor)
        {
            isSelected = false;
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor ends selection of an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the selection.</param>
        /// <seealso cref="OnSelectEntered"/>
        /// <seealso cref="OnSelectCanceled"/>
        protected internal virtual void OnSelectExited(XRBaseInteractor interactor)
        {
            m_OnSelectExited?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// right before the interactor targeting this interactable is disabled or destroyed,
        /// or if the interactable is unregistered.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the selection.</param>
        /// <seealso cref="OnSelectEntered"/>
        /// <seealso cref="OnSelectExited"/>
        protected internal virtual void OnSelectCanceling(XRBaseInteractor interactor)
        {
             isSelected = false;
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor targeting this interactable is disabled or destroyed,
        /// or if the interactable is unregistered.
        /// </summary>
        /// <param name="interactor">Interactor that is ending the selection.</param>
        /// <seealso cref="OnSelectEntered"/>
        /// <seealso cref="OnSelectExited"/>
        protected internal virtual void OnSelectCanceled(XRBaseInteractor interactor)
        {
            m_OnSelectCanceled?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager
        /// when the interactor sends an activation event down to an interactable.
        /// </summary>
        /// <param name="interactor">Interactor that is sending the activation event.</param>
        protected internal virtual void OnActivate(XRBaseInteractor interactor)
        {
            m_OnActivate?.Invoke(interactor);
        }

        public virtual void AttachCustomReticle(XRBaseInteractor interactor)
        {
            if (interactor == null)
                return;

            // Try and find any attached reticle and swap it
            var reticleProvider = interactor.GetComponent<IXRCustomReticleProvider>();
            if (reticleProvider != null)
            {
                if (m_ReticleCache.TryGetValue(interactor, out var prevReticle))
                {
                    Destroy(prevReticle);
                    m_ReticleCache.Remove(interactor);
                }

                if (m_CustomReticle != null)
                {
                    var reticleInstance = Instantiate(m_CustomReticle);
                    m_ReticleCache.Add(interactor, reticleInstance);
                    reticleProvider.AttachCustomReticle(reticleInstance);
                }
            }
        }

        public virtual void RemoveCustomReticle(XRBaseInteractor interactor)
        {
            if (interactor == null)
                return;

            // Try and find any attached reticle and swap it
            var reticleProvider = interactor.transform.GetComponent<IXRCustomReticleProvider>();
            if (reticleProvider != null)
            {
                if (m_ReticleCache.TryGetValue(interactor, out var reticle))
                {
                    Destroy(reticle);
                    m_ReticleCache.Remove(interactor);
                    reticleProvider.RemoveCustomReticle();
                }
            }
        }

        protected internal virtual void OnDeactivate(XRBaseInteractor interactor)
        {
            m_OnDeactivate?.Invoke(interactor);
        }

        /// <summary>
        /// This method is called by the interaction manager to update the interactable.
        /// Please see the interaction manager documentation for more details on update order.
        /// </summary>
        public virtual void ProcessInteractable(XRInteractionUpdateOrder.UpdatePhase updatePhase) {}
    }
}
