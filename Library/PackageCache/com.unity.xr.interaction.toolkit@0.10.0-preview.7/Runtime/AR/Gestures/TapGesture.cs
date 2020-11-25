//-----------------------------------------------------------------------
// <copyright file="TapGesture.cs" company="Google">
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

#if AR_FOUNDATION_PRESENT

using UnityEngine;

namespace UnityEngine.XR.Interaction.Toolkit.AR
{
    /// <summary>
    /// Gesture for when the user performs a tap on the touch screen.
    /// </summary>
    public class TapGesture : Gesture<TapGesture>
    {
        float m_ElapsedTime = 0.0f;

        /// <summary>
        /// Constructs a Tap gesture.
        /// </summary>
        /// <param name="recognizer">The gesture recognizer.</param>
        /// <param name="touch">The touch that started this gesture.</param>
        internal TapGesture(TapGestureRecognizer recognizer, Touch touch) : base(recognizer)
        {
            fingerId = touch.fingerId;
            startPosition = touch.position;
        }

        /// <summary>
        /// (Read Only) The id of the finger used in this gesture.
        /// </summary>
        public int fingerId { get; }

        /// <summary>
        /// (Read Only) The screen position where the gesture started.
        /// </summary>
        public Vector2 startPosition { get; }

        /// <summary>
        /// Returns true if this gesture can start.
        /// </summary>
        /// <returns>Returns <see langword="true"/> if the gesture can start. Returns <see langword="false"/> otherwise.</returns>
        protected internal override bool CanStart()
        {
            if (GestureTouchesUtility.IsFingerIdRetained(fingerId))
            {
                Cancel();
                return false;
            }

            return true;
        }

        /// <summary>
        /// Action to be performed when this gesture is started.
        /// </summary>
        protected internal override void OnStart()
        {
            RaycastHit hit;
            if (GestureTouchesUtility.RaycastFromCamera(startPosition, out hit))
            {
                var gameObject = hit.transform.gameObject;
                if (gameObject != null)
                {
                    var interactableObject = gameObject.GetComponentInParent<ARBaseGestureInteractable>();
                    if (interactableObject != null)
                        TargetObject = interactableObject.gameObject;
                    else if (gameObject.layer == 9)
                        TargetObject = gameObject;
                }
            }
        }

        /// <summary>
        /// Updates this gesture.
        /// </summary>
        /// <returns>Returns <see langword="true"/> if the update was successful. Returns <see langword="false"/> otherwise.</returns>
        protected internal override bool UpdateGesture()
        {
            if (GestureTouchesUtility.TryFindTouch(fingerId, out var touch))
            {
                var tapRecognizer = m_Recognizer as TapGestureRecognizer;
                m_ElapsedTime += touch.deltaTime;
                if (m_ElapsedTime > tapRecognizer.m_TimeSeconds)
                {
                    Cancel();
                }
                else if (touch.phase == TouchPhase.Moved)
                {
                    var diff = (touch.position - startPosition).magnitude;
                    var diffInches = GestureTouchesUtility.PixelsToInches(diff);
                    if (diffInches > tapRecognizer.m_SlopInches)
                    {
                        Cancel();
                    }
                }
                else if (touch.phase == TouchPhase.Ended)
                {
                    Complete();
                }
            }
            else
            {
                Cancel();
            }

            return false;
        }

        /// <summary>
        /// Action to be performed when this gesture is cancelled.
        /// </summary>
        protected internal override void OnCancel()
        {
        }

        /// <summary>
        /// Action to be performed when this gesture is finished.
        /// </summary>
        protected internal override void OnFinish()
        {
        }
    }
}

#endif
