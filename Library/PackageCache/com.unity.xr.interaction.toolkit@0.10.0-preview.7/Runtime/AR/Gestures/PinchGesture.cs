//-----------------------------------------------------------------------
// <copyright file="PinchGesture.cs" company="Google">
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
    /// Gesture for when the user performs a two-finger pinch motion on the touch screen.
    /// </summary>
    public class PinchGesture : Gesture<PinchGesture>
    {
        /// <summary>
        /// Constructs a PinchGesture gesture.
        /// </summary>
        /// <param name="recognizer">The gesture recognizer.</param>
        /// <param name="touch1">The first touch that started this gesture.</param>
        /// <param name="touch2">The second touch that started this gesture.</param>
        public PinchGesture(PinchGestureRecognizer recognizer, Touch touch1, Touch touch2) :
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
        /// (Read Only) The gap between then position of the first and second fingers.
        /// </summary>
        public float gap { get; private set; }

        /// <summary>
        /// (Read Only) The gap delta between then position of the first and second fingers.
        /// </summary>
        public float gapDelta { get; private set; }

        /// <summary>
        /// Returns true if this gesture can start.
        /// </summary>
        /// <returns>Returns <see langword="true"/> if the gesture can start. Returns <see langword="false"/> otherwise.</returns>
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

            // Check that at least one finger is moving.
            if (touch1.deltaPosition == Vector2.zero && touch2.deltaPosition == Vector2.zero)
            {
                return false;
            }

            var pinchRecognizer = m_Recognizer as PinchGestureRecognizer;

            Vector3 firstToSecondDirection = (startPosition1 - startPosition2).normalized;
            var dot1 = Vector3.Dot(touch1.deltaPosition.normalized, -firstToSecondDirection);
            var dot2 = Vector3.Dot(touch2.deltaPosition.normalized, firstToSecondDirection);
            var dotThreshold = Mathf.Cos(pinchRecognizer.m_SlopMotionDirectionDegrees * Mathf.Deg2Rad);

            // Check angle of motion for the first touch.
            if (touch1.deltaPosition != Vector2.zero && Mathf.Abs(dot1) < dotThreshold)
            {
                return false;
            }

            // Check angle of motion for the second touch.
            if (touch2.deltaPosition != Vector2.zero && Mathf.Abs(dot2) < dotThreshold)
            {
                return false;
            }

            var startgap = (startPosition1 - startPosition2).magnitude;
            gap = (touch1.position - touch2.position).magnitude;
            var separation = GestureTouchesUtility.PixelsToInches(Mathf.Abs(gap - startgap));
            return !(separation < pinchRecognizer.m_SlopInches);
        }

        /// <summary>
        /// Action to be performed when this gesture is started.
        /// </summary>
        protected internal override void OnStart()
        {
            GestureTouchesUtility.LockFingerId(fingerId1);
            GestureTouchesUtility.LockFingerId(fingerId2);
        }

        /// <summary>
        /// Updates this gesture.
        /// </summary>
        /// <returns>True if the update was successful.</returns>
        protected internal override bool UpdateGesture()
        {
            Touch touch1, touch2;
            bool foundTouches = GestureTouchesUtility.TryFindTouch(fingerId1, out touch1);
            foundTouches =
                GestureTouchesUtility.TryFindTouch(fingerId2, out touch2) && foundTouches;

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
                float newgap = (touch1.position - touch2.position).magnitude;
                gapDelta = newgap - gap;
                gap = newgap;
                return true;
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
            GestureTouchesUtility.ReleaseFingerId(fingerId1);
            GestureTouchesUtility.ReleaseFingerId(fingerId2);
        }
    }
}

#endif
