//-----------------------------------------------------------------------
// <copyright file="DragGesture.cs" company="Google">
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

using System;
using UnityEngine;

namespace UnityEngine.XR.Interaction.Toolkit.AR
{
    /// <summary>
    /// Gesture for when the user performs a drag motion on the touch screen.
    /// </summary>
    public class DragGesture : Gesture<DragGesture>
    {
        /// <summary>
        /// Constructs a DragGesture gesture.
        /// </summary>
        /// <param name="recognizer">The gesture recognizer.</param>
        /// <param name="touch">The touch that started this gesture.</param>
        public DragGesture(DragGestureRecognizer recognizer, Touch touch) : base(recognizer)
        {
            fingerId = touch.fingerId;
            startPosition = touch.position;
            position = startPosition;
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
        /// (Read Only) The current screen position of the gesture.
        /// </summary>
        public Vector2 position { get; private set; }

        /// <summary>
        /// (Read Only) The delta screen position of the gesture.
        /// </summary>
        public Vector2 delta { get; private set; }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("FingerId has been deprecated. Use fingerId instead. (UnityUpgradable) -> fingerId")]
        public int FingerId => fingerId;
        [Obsolete("StartPosition has been deprecated. Use startPosition instead. (UnityUpgradable) -> startPosition")]
        public Vector2 StartPosition => startPosition;
        [Obsolete("Position has been deprecated. Use position instead. (UnityUpgradable) -> position")]
        public Vector2 Position => position;
        [Obsolete("Delta has been deprecated. Use delta instead. (UnityUpgradable) -> delta")]
        public Vector2 Delta => delta;
#pragma warning restore IDE1006

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

            if (GestureTouchesUtility.Touches.Length > 1)
            {
                for (int i = 0; i < GestureTouchesUtility.Touches.Length; i++)
                {
                    var currentTouch = GestureTouchesUtility.Touches[i];
                    if (currentTouch.fingerId != fingerId
                        && !GestureTouchesUtility.IsFingerIdRetained(currentTouch.fingerId))
                    {
                        return false;
                    }
                }
            }

            if (GestureTouchesUtility.TryFindTouch(fingerId, out var touch))
            {
                var pos = touch.position;
                var diff = (pos - startPosition).magnitude;
                if (GestureTouchesUtility.PixelsToInches(diff) >= (m_Recognizer as DragGestureRecognizer).m_SlopInches)
                {
                    return true;
                }
            }
            else
            {
                Cancel();
            }

            return false;
        }

        /// <summary>
        /// Action to be performed when this gesture is started.
        /// </summary>
        protected internal override void OnStart()
        {
            GestureTouchesUtility.LockFingerId(fingerId);

            if (GestureTouchesUtility.RaycastFromCamera(startPosition, out var hit))
            {
                var gameObject = hit.transform.gameObject;
                if (gameObject != null)
                {
                    var interactableObject = gameObject.GetComponentInParent<ARBaseGestureInteractable>();
                    if (interactableObject != null)
                        TargetObject = interactableObject.gameObject;
                }
            }

            GestureTouchesUtility.TryFindTouch(fingerId, out var touch);
            position = touch.position;
        }

        /// <summary>
        /// Updates this gesture.
        /// </summary>
        /// <returns>Returns <see langword="true"/> if the update was successful. Returns <see langword="false"/> otherwise.</returns>
        protected internal override bool UpdateGesture()
        {
            if (GestureTouchesUtility.TryFindTouch(fingerId, out var touch))
            {
                if (touch.phase == TouchPhase.Moved)
                {
                    delta = touch.position - position;
                    position = touch.position;
                    return true;
                }
                else if (touch.phase == TouchPhase.Ended)
                {
                    Complete();
                }
                else if (touch.phase == TouchPhase.Canceled)
                {
                    Cancel();
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
        protected internal override void OnCancel() { }

        /// <summary>
        /// Action to be performed when this gesture is finished.
        /// </summary>
        protected internal override void OnFinish() => GestureTouchesUtility.ReleaseFingerId(fingerId);
    }
}

#endif
