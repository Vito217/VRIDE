using System;
using System.Collections.Generic;
using System.Linq;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Interactor used for directly interacting with interactables that are touching. This is handled via trigger volumes
    /// that update the current set of valid targets for this interactor. This component must have a collision volume that is 
    /// set to be a trigger to work.
    /// </summary>
    [DisallowMultipleComponent]
    [AddComponentMenu("XR/XR Direct Interactor")]
    public class XRDirectInteractor : XRBaseControllerInteractor
    {
        readonly List<XRBaseInteractable> m_ValidTargets = new List<XRBaseInteractable>();
        /// <inheritdoc />
        protected override List<XRBaseInteractable> validTargets => m_ValidTargets;

        /// <summary>
        /// Reusable map of interactables to their distance squared from this interactor (used for sort).
        /// </summary>
        readonly Dictionary<XRBaseInteractable, float> m_InteractableDistanceSqrMap = new Dictionary<XRBaseInteractable, float>();

        /// <summary>
        /// Sort comparison function used by <see cref="GetValidTargets"/>.
        /// </summary>
        Comparison<XRBaseInteractable> m_InteractableSortComparison;

        protected override void Awake()
        {
            base.Awake();

            m_InteractableSortComparison = InteractableSortComparison;
            if (!GetComponents<Collider>().Any(x => x.isTrigger))
                Debug.LogWarning("Direct Interactor does not have required Collider set as a trigger.", this);
        }

        protected void OnTriggerEnter(Collider col)
        {
            var interactable = interactionManager.TryGetInteractableForCollider(col);
            if (interactable != null && !m_ValidTargets.Contains(interactable))
                m_ValidTargets.Add(interactable);
        }

        protected void OnTriggerExit(Collider col)
        {
            var interactable = interactionManager.TryGetInteractableForCollider(col);
            if (interactable != null)
                m_ValidTargets.Remove(interactable);
        }

        /// <summary>
        /// Retrieve the list of interactables that this interactor could possibly interact with this frame.
        /// This list is sorted by priority (in this case distance).
        /// </summary>
        /// <param name="validTargets">Populated List of interactables that are valid for selection or hover.</param>
        public override void GetValidTargets(List<XRBaseInteractable> validTargets)
        {
            validTargets.Clear();
            m_InteractableDistanceSqrMap.Clear();

            // Calculate distance squared to interactor's attach transform and add to validTargets (which is sorted before returning)
            foreach (var interactable in m_ValidTargets)
            {
                m_InteractableDistanceSqrMap[interactable] = interactable.GetDistanceSqrToInteractor(this);
                validTargets.Add(interactable);
            }

            validTargets.Sort(m_InteractableSortComparison);
        }

        /// <inheritdoc />
        public override bool CanHover(XRBaseInteractable interactable)
        {
            return base.CanHover(interactable) && (selectTarget == null || selectTarget == interactable);
        }

        /// <inheritdoc />
        public override bool CanSelect(XRBaseInteractable interactable)
        {
            return base.CanSelect(interactable) && (selectTarget == null || selectTarget == interactable);
        }

        int InteractableSortComparison(XRBaseInteractable x, XRBaseInteractable y)
        {
            var xDistance = m_InteractableDistanceSqrMap[x];
            var yDistance = m_InteractableDistanceSqrMap[y];
            if (xDistance > yDistance)
                return 1;
            if (xDistance < yDistance)
                return -1;

            return 0;
        }
    }
}