using System.Collections.Generic;
using UnityEngine.XR.Interaction.Toolkit.AR;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The Interaction Manager acts as an intermediary between Interactors and Interactables.
    /// It is possible to have multiple Interaction Managers, each with their own valid set of Interactors and Interactables.
    /// Upon Awake both Interactors and Interactables register themselves with a valid Interaction Manager
    /// (if a specific one has not already been assigned in the inspector). The loaded scenes must have at least one Interaction Manager
    /// for Interactors and Interactables to be able to communicate.
    /// </summary>
    /// <remarks>
    /// Many of the methods on this class are designed to be internal such that they can be called by the abstract
    /// base classes of the Interaction system (but are not called directly).
    /// </remarks>
    [AddComponentMenu("XR/XR Interaction Manager")]
    [DisallowMultipleComponent]
    [DefaultExecutionOrder(XRInteractionUpdateOrder.k_InteractionManager)]
    public class XRInteractionManager : MonoBehaviour
    {
        // TODO Expose as a read-only wrapper without using ReadOnlyCollection since that class causes allocations when enumerating
        /// <summary>
        /// (Read Only) List of registered interactors.
        /// </summary>
        /// <remarks>
        /// Intended to be used by XR Interaction Debugger and tests.
        /// </remarks>
        internal List<XRBaseInteractor> interactors => m_Interactors;

        // TODO Expose as a read-only wrapper without using ReadOnlyCollection since that class causes allocations when enumerating
        /// <summary>
        /// (Read Only) List of registered interactables.
        /// </summary>
        /// <remarks>
        /// Intended to be used by XR Interaction Debugger and tests.
        /// </remarks>
        internal List<XRBaseInteractable> interactables => m_Interactables;

        /// <summary>
        /// Map of all registered objects to test for colliding.
        /// </summary>
        readonly Dictionary<Collider, XRBaseInteractable> m_ColliderToInteractableMap = new Dictionary<Collider, XRBaseInteractable>();

        /// <summary>
        /// List of registered interactors.
        /// </summary>
        readonly List<XRBaseInteractor> m_Interactors = new List<XRBaseInteractor>();

        /// <summary>
        /// List of registered interactables.
        /// </summary>
        readonly List<XRBaseInteractable> m_Interactables = new List<XRBaseInteractable>();

        /// <summary>
        /// Reusable list of interactables for retrieving hover targets.
        /// </summary>
        readonly List<XRBaseInteractable> m_HoverTargetList = new List<XRBaseInteractable>();

        /// <summary>
        /// Reusable list of valid targets for an interactor.
        /// </summary>
        readonly List<XRBaseInteractable> m_InteractorValidTargets = new List<XRBaseInteractable>();

#if AR_FOUNDATION_PRESENT
        /// <summary>
        /// A boolean value that indicates that interactables should be reconnected to gestures next frame.
        /// </summary>
        bool m_GestureInteractablesNeedReconnect;
#endif

        protected virtual void OnEnable()
        {
            Application.onBeforeRender += OnBeforeRender;
        }

        protected virtual void OnDisable()
        {
            Application.onBeforeRender -= OnBeforeRender;
        }

        protected virtual void Update()
        {
            ProcessInteractors(XRInteractionUpdateOrder.UpdatePhase.Dynamic);

            foreach (var interactor in m_Interactors)
            {
                GetValidTargets(interactor, m_InteractorValidTargets);

                ClearInteractorSelection(interactor);
                ClearInteractorHover(interactor, m_InteractorValidTargets);
                InteractorSelectValidTargets(interactor, m_InteractorValidTargets);
                InteractorHoverValidTargets(interactor, m_InteractorValidTargets);
            }

            ProcessInteractables(XRInteractionUpdateOrder.UpdatePhase.Dynamic);

#if AR_FOUNDATION_PRESENT
            // Check if gesture interactors/interactables have been updated
            // (in which case we need to reconnect gestures).
            if (m_GestureInteractablesNeedReconnect)
            {
                foreach (var interactable in m_Interactables)
                {
                    var gestureInteractable = interactable as ARBaseGestureInteractable;
                    if (gestureInteractable != null)
                    {
                        gestureInteractable.DisconnectGestureInteractor();
                        gestureInteractable.ConnectGestureInteractor();
                    }
                }

                m_GestureInteractablesNeedReconnect = false;
            }
#endif
        }

        protected virtual void LateUpdate()
        {
            ProcessInteractors(XRInteractionUpdateOrder.UpdatePhase.Late);
            ProcessInteractables(XRInteractionUpdateOrder.UpdatePhase.Late);
        }

        protected virtual void FixedUpdate()
        {
            ProcessInteractors(XRInteractionUpdateOrder.UpdatePhase.Fixed);
            ProcessInteractables(XRInteractionUpdateOrder.UpdatePhase.Fixed);
        }

        [BeforeRenderOrder(XRInteractionUpdateOrder.k_BeforeRenderOrder)]
        protected virtual void OnBeforeRender()
        {
            ProcessInteractors(XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender);
            ProcessInteractables(XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender);
        }

        protected virtual void ProcessInteractors(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            foreach (var interactor in m_Interactors)
            {
                interactor.ProcessInteractor(updatePhase);
            }
        }

        protected virtual void ProcessInteractables(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            foreach (var interactable in m_Interactables)
            {
                interactable.ProcessInteractable(updatePhase);
            }
        }

        public virtual void RegisterInteractor(XRBaseInteractor interactor)
        {
            if (!m_Interactors.Contains(interactor))
            {
                m_Interactors.Add(interactor);
#if AR_FOUNDATION_PRESENT
                if (interactor is ARGestureInteractor)
                    m_GestureInteractablesNeedReconnect = true;
#endif
            }
        }

        public virtual void UnregisterInteractor(XRBaseInteractor interactor)
        {
            if (m_Interactors.Contains(interactor))
            {
                ClearInteractorHover(interactor, null);
                SelectCancel(interactor, interactor.selectTarget);

                m_Interactors.Remove(interactor);
#if AR_FOUNDATION_PRESENT
                if (interactor is ARGestureInteractor)
                    m_GestureInteractablesNeedReconnect = true;
#endif
            }
        }

        public virtual void RegisterInteractable(XRBaseInteractable interactable)
        {
            if (!m_Interactables.Contains(interactable))
            {
                m_Interactables.Add(interactable);

                foreach (var interactableCollider in interactable.colliders)
                {
                    if (interactableCollider != null && !m_ColliderToInteractableMap.ContainsKey(interactableCollider))
                        m_ColliderToInteractableMap.Add(interactableCollider, interactable);
                }
#if AR_FOUNDATION_PRESENT
                if (interactable is ARBaseGestureInteractable)
                    m_GestureInteractablesNeedReconnect = true;
#endif
            }
        }

        public virtual void UnregisterInteractable(XRBaseInteractable interactable)
        {
            if (m_Interactables.Contains(interactable))
            {
                // Cancel select states for interactors that are selecting this interactable.
                foreach (var interactor in m_Interactors)
                {
                    if (interactor.selectTarget == interactable)
                        SelectCancel(interactor, interactable);
                }

                m_Interactables.Remove(interactable);

                foreach (var interactableCollider in interactable.colliders)
                {
                    if (interactableCollider != null && m_ColliderToInteractableMap.ContainsKey(interactableCollider))
                        m_ColliderToInteractableMap.Remove(interactableCollider);
                }
#if AR_FOUNDATION_PRESENT
                if (interactable is ARBaseGestureInteractable gestureInteractable)
                {
                    gestureInteractable.DisconnectGestureInteractor();
                    m_GestureInteractablesNeedReconnect = true;
                }
#endif
            }
        }

        public XRBaseInteractable TryGetInteractableForCollider(Collider interactableCollider)
        {
            if (interactableCollider != null && m_ColliderToInteractableMap.TryGetValue(interactableCollider, out var interactable))
                return interactable;

            return null;
        }

        // TODO Expose the dictionary as a read-only wrapper if necessary rather than providing a reference this way
        public void GetColliderToInteractableMap(ref Dictionary<Collider, XRBaseInteractable> map)
        {
            if (map != null)
            {
                map.Clear();
                map = m_ColliderToInteractableMap;
            }
        }

        public List<XRBaseInteractable> GetValidTargets(XRBaseInteractor interactor, List<XRBaseInteractable> validTargets)
        {
            interactor.GetValidTargets(validTargets);

            // Remove interactables that are not being handled by this manager.
            for (var i = validTargets.Count - 1; i >= 0; --i)
            {
                if (!m_Interactables.Contains(validTargets[i]))
                    validTargets.RemoveAt(i);
            }

            return validTargets;
        }

        public void ForceSelect(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            SelectEnter(interactor, interactable);
        }

        public virtual void ClearInteractorSelection(XRBaseInteractor interactor)
        {
            if (interactor.selectTarget != null &&
                (!interactor.isSelectActive || !interactor.CanSelect(interactor.selectTarget) || !interactor.selectTarget.IsSelectableBy(interactor)))
            {
                SelectExit(interactor, interactor.selectTarget);
            }
        }

        public virtual void ClearInteractorHover(XRBaseInteractor interactor, List<XRBaseInteractable> validTargets)
        {
            interactor.GetHoverTargets(m_HoverTargetList);
            foreach (var target in m_HoverTargetList)
            {
                if (!interactor.isHoverActive || !interactor.CanHover(target) || !target.IsHoverableBy(interactor) || ((validTargets != null && !validTargets.Contains(target)) || validTargets == null))
                    HoverExit(interactor, target);
            }
        }

        public virtual void SelectEnter(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            // If Exclusive Selection, is this the only interactor trying to interact?
            if (interactor.requireSelectExclusive)
            {
                for (var i = 0; i < m_Interactors.Count; ++i)
                {
                    if (m_Interactors[i] != interactor
                        && m_Interactors[i].selectTarget == interactable)
                    {
                        return;
                    }
                }
            }
            else
            {
                for (var i = 0; i < m_Interactors.Count; ++i)
                {
                    if (m_Interactors[i].selectTarget == interactable)
                        SelectExit(m_Interactors[i], interactable);
                }
            }

            interactor.OnSelectEntering(interactable);
            interactable.OnSelectEntering(interactor);
            interactor.OnSelectEntered(interactable);
            interactable.OnSelectEntered(interactor);
        }

        public virtual void SelectExit(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            interactor.OnSelectExiting(interactable);
            interactable.OnSelectExiting(interactor);
            interactor.OnSelectExited(interactable);
            interactable.OnSelectExited(interactor);
        }

        public virtual void SelectCancel(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            // TODO Add an OnSelectCancel function to XRBaseInteractor?
            interactor.OnSelectExiting(interactable);
            interactor.OnSelectExited(interactable);
            if (interactable != null)
            {
                interactable.OnSelectCanceling(interactor);
                interactable.OnSelectCanceled(interactor);
            }
        }

        public virtual void HoverEnter(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            interactor.OnHoverEntering(interactable);
            interactable.OnHoverEntering(interactor);
            interactor.OnHoverEntered(interactable);
            interactable.OnHoverEntered(interactor);
        }

        public virtual void HoverExit(XRBaseInteractor interactor, XRBaseInteractable interactable)
        {
            interactor.OnHoverExiting(interactable);
            interactable.OnHoverExiting(interactor);
            interactor.OnHoverExited(interactable);
            interactable.OnHoverExited(interactor);
        }

        protected virtual void InteractorSelectValidTargets(XRBaseInteractor interactor, List<XRBaseInteractable> validTargets)
        {
            if (interactor.isSelectActive)
            {
                for (var i = 0; i < validTargets.Count && interactor.isSelectActive; ++i)
                {
                    if (interactor.CanSelect(validTargets[i]) && validTargets[i].IsSelectableBy(interactor) &&
                        interactor.selectTarget != validTargets[i])
                    {
                        SelectEnter(interactor, validTargets[i]);
                    }
                }
            }
        }

        protected virtual void InteractorHoverValidTargets(XRBaseInteractor interactor, List<XRBaseInteractable> validTargets)
        {
            if (interactor.isHoverActive)
            {
                for (var i=0; i < validTargets.Count && interactor.isHoverActive; ++i)
                {
                    interactor.GetHoverTargets(m_HoverTargetList);
                    if (interactor.CanHover(validTargets[i]) && validTargets[i].IsHoverableBy(interactor) &&
                        !m_HoverTargetList.Contains(validTargets[i]))
                    {
                        HoverEnter(interactor, validTargets[i]);
                    }
                }
            }
        }
    }
}
