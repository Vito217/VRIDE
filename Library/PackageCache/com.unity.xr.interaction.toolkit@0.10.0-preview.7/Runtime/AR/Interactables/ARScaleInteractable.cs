//-----------------------------------------------------------------------
// <copyright file="ScaleManipulator.cs" company="Google">
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
namespace UnityEngine.XR.Interaction.Toolkit.AR {  public class ARScaleInteractable {} }

#else

using UnityEngine;

namespace UnityEngine.XR.Interaction.Toolkit.AR
{
    /// <summary>
    /// Controls the scale of an object via a Pinch gesture.
    /// If an object is selected, then doing a pinch/zoom modify the scale
    /// of the object.
    /// </summary>
    public class ARScaleInteractable : ARBaseGestureInteractable
    {
        [SerializeField, Tooltip("The minimum scale of the object.")]
        float m_MinScale = 0.75f;

        /// <summary>
        /// The minimum scale of the object.
        /// </summary>
        public float minScale
        {
            get => m_MinScale;
            set => m_MinScale = value;
        }

        [SerializeField, Tooltip("The maximum scale of the object.")]
        float m_MaxScale = 1.75f;

        /// <summary>
        /// The maximum scale of the object.
        /// </summary>
        public float maxScale
        {
            get => m_MaxScale;
            set => m_MaxScale = value;
        }

        [SerializeField, Tooltip("The elastic ratio used when scaling the object")]
        float m_ElasticRatioLimit;

        /// <summary>
        /// The limit of the elastic ratio.
        /// </summary>
        public float elasticRatioLimit
        {
            get => m_ElasticRatioLimit;
            set => m_ElasticRatioLimit = value;
        }

        [SerializeField, Tooltip("Sensitivity to movement being translated into scale.")]
        float m_Sensitivity = 0.75f;

        /// <summary>
        /// Sensitivity to movement being translated into scale.
        /// </summary>
        public float sensitivity
        {
            get => m_Sensitivity;
            set => m_Sensitivity = value;
        }

        [SerializeField, Tooltip("Amount that the scale bounces back after hitting min/max of range.")]
        float m_Elasticity = 0.15f;

        /// <summary>
        /// Amount that the scale bounces back after hitting min/max of range.
        /// </summary>
        public float elasticity
        {
            get => m_Elasticity;
            set => m_Elasticity = value;
        }

        float scaleDelta
        {
            get
            {
                if (minScale > maxScale)
                {
                    Debug.LogError("minScale must be smaller than maxScale.");
                    return 0f;
                }

                return maxScale - minScale;
            }
        }

        float clampedScaleRatio => Mathf.Clamp01(m_CurrentScaleRatio);

        Vector3 currentScale
        {
            get
            {
                var elasticScaleRatio = clampedScaleRatio + ElasticDelta();
                var elasticScale = minScale + (elasticScaleRatio * scaleDelta);
                return new Vector3(elasticScale, elasticScale, elasticScale);
            }
        }

        float m_CurrentScaleRatio;
        bool m_IsScaling;

        protected void OnValidate()
        {
            minScale = Mathf.Max(0f, minScale);
            maxScale = Mathf.Max(Mathf.Max(0f, minScale), maxScale);
        }

        /// <summary>
        /// Enabled the scale controller.
        /// </summary>
        protected void OnEnable()
        {
            m_CurrentScaleRatio = (transform.localScale.x - minScale) / scaleDelta;
        }

        protected void LateUpdate()
        {
            if (!m_IsScaling)
            {
                m_CurrentScaleRatio =
                    Mathf.Lerp(m_CurrentScaleRatio, clampedScaleRatio, Time.deltaTime * 8f);
                transform.localScale = currentScale;
            }
        }

        /// <summary>
        /// Returns true if the manipulation can be started for the given gesture.
        /// </summary>
        /// <param name="gesture">The current gesture.</param>
        /// <returns>Returns <see langword="true"/> if the manipulation can be started. Returns <see langword="false"/> otherwise.</returns>
        protected override bool CanStartManipulationForGesture(PinchGesture gesture)
        {
            if (!IsGameObjectSelected())
            {
                return false;
            }

            if (gesture.TargetObject != null)
            {
                return false;
            }

            return true;
        }

        /// <summary>
        /// Recalculates the current scale ratio in case local scale, min or max scale were changed.
        /// </summary>
        /// <param name="gesture">The gesture that started this transformation.</param>
        protected override void OnStartManipulation(PinchGesture gesture)
        {
            m_IsScaling = true;
            m_CurrentScaleRatio = (transform.localScale.x - minScale) / scaleDelta;
        }

        /// <summary>
        /// Continues the scaling of the object.
        /// </summary>
        /// <param name="gesture">The current gesture.</param>
        protected override void OnContinueManipulation(PinchGesture gesture)
        {
            m_CurrentScaleRatio += sensitivity * GestureTouchesUtility.PixelsToInches(gesture.gapDelta);

            transform.localScale = currentScale;

            // If we've tried to scale too far beyond the limit, then cancel the gesture
            // to snap back within the scale range.
            if (m_CurrentScaleRatio < -elasticRatioLimit
                || m_CurrentScaleRatio > (1f + elasticRatioLimit))
            {
                gesture.Cancel();
            }
        }

        /// <summary>
        /// Finishes the scaling of the object.
        /// </summary>
        /// <param name="gesture">The current gesture.</param>
        protected override void OnEndManipulation(PinchGesture gesture)
        {
            m_IsScaling = false;
        }

        float ElasticDelta()
        {
            float overRatio;
            if (m_CurrentScaleRatio > 1f)
            {
                overRatio = m_CurrentScaleRatio - 1f;
            }
            else if (m_CurrentScaleRatio < 0f)
            {
                overRatio = m_CurrentScaleRatio;
            }
            else
            {
                return 0f;
            }

            return (1f - (1f / ((Mathf.Abs(overRatio) * elasticity) + 1f))) * Mathf.Sign(overRatio);
        }
    }
}

#endif
