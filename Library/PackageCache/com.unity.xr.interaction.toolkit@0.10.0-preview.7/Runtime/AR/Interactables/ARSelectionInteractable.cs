//-----------------------------------------------------------------------
// <copyright file="SelectionManipulator.cs" company="Google">
//
// Copyright 2018 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// </copyright>
//-----------------------------------------------------------------------

// Modifications copyright © 2020 Unity Technologies ApS

#if !AR_FOUNDATION_PRESENT

// Stub class definition used to fool version defines that this MonoScript exists (fixed in 19.3)
namespace UnityEngine.XR.Interaction.Toolkit.AR {  public class ARSelectionInteractable {} }

#else

namespace UnityEngine.XR.Interaction.Toolkit.AR
{
    /// <summary>
    /// Controls the selection of an object through Tap gesture.
    /// </summary>
    public class ARSelectionInteractable : ARBaseGestureInteractable
    {
        [SerializeField, Tooltip("The visualization GameObject that will become active when the object is selected.")]
        GameObject m_SelectionVisualization;
        /// <summary>
        /// The visualization <see cref="GameObject"/> that will become active when the object is selected.
        /// </summary>
        public GameObject selectionVisualization
        {
            get => m_SelectionVisualization;
            set => m_SelectionVisualization = value;
        }

        bool m_GestureSelected;

        /// <inheritdoc />
        public override bool IsSelectableBy(XRBaseInteractor interactor)
        {
            if (!(interactor is ARGestureInteractor))
                return false;

            return m_GestureSelected;
        }

        /// <inheritdoc />
        protected override bool CanStartManipulationForGesture(TapGesture gesture) => true;

        /// <inheritdoc />
        protected override void OnEndManipulation(TapGesture gesture)
        {
            base.OnEndManipulation(gesture);

            if (gesture.WasCancelled)
                return;
            if (gestureInteractor == null)
                return;

            if (gesture.TargetObject == gameObject)
            {
                // Toggle selection
                m_GestureSelected = !m_GestureSelected;
            }
            else
                m_GestureSelected = false;
        }

        /// <inheritdoc />
        protected internal override void OnSelectEntering(XRBaseInteractor interactor)
        {
            base.OnSelectEntering(interactor);
            if (m_SelectionVisualization != null)
                m_SelectionVisualization.SetActive(true);
        }

        /// <inheritdoc />
        protected internal override void OnSelectExiting(XRBaseInteractor interactor)
        {
            base.OnSelectExiting(interactor);

            if (m_SelectionVisualization != null)
                m_SelectionVisualization.SetActive(false);
        }

        /// <inheritdoc />
        protected internal override void OnSelectCanceling(XRBaseInteractor interactor)
        {
            base.OnSelectCanceling(interactor);
            if (m_SelectionVisualization != null)
                m_SelectionVisualization.SetActive(false);
        }
    }
}

#endif
