//-----------------------------------------------------------------------
// <copyright file="TwistGesture.cs" company="Google">
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
    /// Gesture for when the user performs a two-finger twist motion on the touch screen.
    /// </summary>
    public class TwistGesture : Gesture<TwistGesture>
    {
        Vector2 m_PreviousPosition1;
        Vector2 m_PreviousPosition2;

        /// <summary>
        /// Constructs a PinchGesture gesture.
        /// </summary>
        /// <param name="recognizer">The gesture recognizer.</param>
        /// <param name="touch1">The first touch that started this gesture.</param>
        /// <param name="touch2">The second touch that started this gesture.</param>
        public TwistGesture(TwistGestureRecognizer recognizer, Touch touch1, Touch touch2) :
            base(recognizer)
        {
            fingerId1 = touch1.fingerId;
            fingerId2 = touch2.fingerId;
            startPosition1 = touch1.position;
            startPosition2 = touch2.position;
        }

        /// <summary>
        /// (Read Only) The id of the first finger used in this gesture.
        /// </summary>
        public int fingerId1 { get; }

        /// <summary>
        /// (Read Only) The id of the second finger used in this gesture.
        /// </summary>
        public int fingerId2 { get; }

        /// <summary>
        /// (Read Only) The screen position of the first finger where the gesture started.
        /// </summary>
        public Vector2 startPosition1 { get; }

        /// <summary>
        /// (Read Only) The screen position of the second finger where the gesture started.
        /// </summary>
        public Vector2 startPosition2 { get; }

        /// <summary>
        /// (Read Only) The delta rotation of the gesture.
        /// </summary>
        public float deltaRotation { get; private set; }

        /// <summary>
        /// Returns true if this gesture can start.
        /// </summary>
        /// <returns>True if the gesture can start.</returns>
        protected internal override bool CanStart()
        {
            if (GestureTouchesUtility.IsFingerIdRetained(fingerId1) ||
                GestureTouchesUtility.IsFingerIdRetained(fingerId2))
            {
                Cancel();
                return false;
            }

            var foundTouches = GestureTouchesUtility.TryFindTouch(fingerId1, out var touch1);
            foundTouches =
                GestureTouchesUtility.TryFindTouch(fingerId2, out var touch2) && foundTouches;

            if (!foundTouches)
            {
                Cancel();
                return false;
            }

            // Check that both fingers are moving.
            if (touch1.deltaPosition == Vector2.zero || touch2.deltaPosition == Vector2.zero)
            {
                return false;
            }

            var twistRecognizer = m_Recognizer as TwistGestureRecognizer;

            var rotation = CalculateDeltaRotation(
                touch1.position, touch2.position, startPosition1, startPosition2);
            return !(Mathf.Abs(rotation) < twistRecognizer.m_SlopRotation);
        }

        /// <summary>
        /// Action to be performed when this gesture is started.
        /// </summary>
        protected internal override void OnStart()
        {
            GestureTouchesUtility.LockFingerId(fingerId1);
            GestureTouchesUtility.LockFingerId(fingerId2);

            GestureTouchesUtility.TryFindTouch(fingerId1, out var touch1);
            GestureTouchesUtility.TryFindTouch(fingerId2, out var touch2);
            m_PreviousPosition1 = touch1.position;
            m_PreviousPosition2 = touch2.position;
        }

        /// <summary>
        /// Updates this gesture.
        /// </summary>
        /// <returns>True if the update was successful.</returns>
        protected internal override bool UpdateGesture()
        {
            var foundTouches = GestureTouchesUtility.TryFindTouch(fingerId1, out var touch1);
            foundTouches =
                GestureTouchesUtility.TryFindTouch(fingerId2, out var touch2) && foundTouches;

            if (!foundTouches)
            {
                Cancel();
                return false;
            }

            if (touch1.phase == TouchPhase.Canceled || touch2.phase == TouchPhase.Canceled)
            {
                Cancel();
                return false;
            }

            if (touch1.phase == TouchPhase.Ended || touch2.phase == TouchPhase.Ended)
            {
                Complete();
                return false;
            }

            if (touch1.phase == TouchPhase.Moved || touch2.phase == TouchPhase.Moved)
            {
                float rotation = CalculateDeltaRotation(
                                     touch1.position,
                                     touch2.position,
                                     m_PreviousPosition1,
                                     m_PreviousPosition2);

                deltaRotation = rotation;
                m_PreviousPosition1 = touch1.position;
                m_PreviousPosition2 = touch2.position;
                return true;
            }

            m_PreviousPosition1 = touch1.position;
            m_PreviousPosition2 = touch2.position;
            deltaRotation = 0.0f;
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
            GestureTouchesUtility.ReleaseFingerId(fingerId1);
            GestureTouchesUtility.ReleaseFingerId(fingerId2);
        }

        protected static float CalculateDeltaRotation(
            Vector2 currentPosition1,
            Vector2 currentPosition2,
            Vector2 previousPosition1,
            Vector2 previousPosition2)
        {
            var currentDirection = (currentPosition1 - currentPosition2).normalized;
            var previousDirection = (previousPosition1 - previousPosition2).normalized;

            var sign = Mathf.Sign((previousDirection.x * currentDirection.y) -
                                    (previousDirection.y * currentDirection.x));
            return Vector2.Angle(currentDirection, previousDirection) * sign;
        }
    }
}

#endif
