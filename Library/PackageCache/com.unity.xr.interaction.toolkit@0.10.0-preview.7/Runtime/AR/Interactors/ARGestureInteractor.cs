//-----------------------------------------------------------------------
// <copyright file="ARGestureInteractor.cs" company="Google">
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
namespace UnityEngine.XR.Interaction.Toolkit.AR {  public class ARGestureInteractor {} }

#else

using System;
using System.Collections.Generic;

namespace UnityEngine.XR.Interaction.Toolkit.AR
{
    /// <summary>
    /// The <see cref="ARGestureInteractor"/> allows the user to manipulate virtual objects (select, translate,
    /// rotate, scale and elevate) through gestures (tap, drag, twist, swipe).
    /// The <see cref="ARGestureInteractor"/> also handles the current selected object and its visualization.
    /// <br />
    /// To enable it add one <see cref="ARGestureInteractor"/> to your scene and one <see cref="ARBaseGestureInteractable"/> as parent of each
    /// of your virtual objects.
    /// </summary>
    public class ARGestureInteractor : XRBaseInteractor
    {
        static ARGestureInteractor s_Instance;
        /// <summary>
        /// (Read Only) The <see cref="ARGestureInteractor"/> instance.
        /// </summary>
        public static ARGestureInteractor instance
        {
            get
            {
                if (s_Instance == null)
                {
                    var xrGestureInteractors = FindObjectsOfType<ARGestureInteractor>();
                    if (xrGestureInteractors.Length > 0)
                    {
                        s_Instance = xrGestureInteractors[0];
                    }
                    else
                    {
                        Debug.LogError("No instance of ARGestureInteractor exists in the scene.");
                    }
                }

                return s_Instance;
            }
        }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("Instance has been deprecated. Use instance instead. (UnityUpgradable) -> instance", true)]
        public static ARGestureInteractor Instance => instance;
#pragma warning restore IDE1006

        DragGestureRecognizer m_DragGestureRecognizer = new DragGestureRecognizer();
        /// <summary>
        /// (Read Only) The Drag gesture recognizer.
        /// </summary>
        public DragGestureRecognizer DragGestureRecognizer => m_DragGestureRecognizer;

        PinchGestureRecognizer m_PinchGestureRecognizer = new PinchGestureRecognizer();
        /// <summary>
        /// (Read Only) The Pinch gesture recognizer.
        /// </summary>
        public PinchGestureRecognizer PinchGestureRecognizer => m_PinchGestureRecognizer;

        TwoFingerDragGestureRecognizer m_TwoFingerDragGestureRecognizer = new TwoFingerDragGestureRecognizer();
        /// <summary>
        /// (Read Only) The two finger drag gesture recognizer.
        /// </summary>
        public TwoFingerDragGestureRecognizer TwoFingerDragGestureRecognizer => m_TwoFingerDragGestureRecognizer;

        TapGestureRecognizer m_TapGestureRecognizer = new TapGestureRecognizer();
        /// <summary>
        /// (Read Only) The Tap gesture recognizer.
        /// </summary>
        public TapGestureRecognizer TapGestureRecognizer => m_TapGestureRecognizer;

        TwistGestureRecognizer m_TwistGestureRecognizer = new TwistGestureRecognizer();
        /// <summary>
        /// (Read Only) The Twist gesture recognizer.
        /// </summary>
        public TwistGestureRecognizer TwistGestureRecognizer => m_TwistGestureRecognizer;

        protected override void Awake()
        {
            base.Awake();

            if (instance != this)
            {
                // TODO We should support multiple ARGestureInteractors eventually
                Debug.LogWarning($"Multiple instances of {nameof(ARGestureInteractor)} detected in the scene." +
                                 " Only one instance can exist at a time. The duplicate instances" +
                                 " will be destroyed.", this);
                DestroyImmediate(gameObject);
            }
        }

        /// <summary>
        /// The Unity Update() method.
        /// </summary>
        public void Update()
        {
            DragGestureRecognizer.Update();
            PinchGestureRecognizer.Update();
            TwoFingerDragGestureRecognizer.Update();
            TapGestureRecognizer.Update();
            TwistGestureRecognizer.Update();
        }

        static float GetHorizontalFOV(Camera camera)
        {
            var vFOV = camera.fieldOfView * Mathf.Deg2Rad;
            var cameraHeight = Mathf.Tan(vFOV * .5f);
            return Mathf.Atan(cameraHeight * camera.aspect);
        }

        /// <summary>
        /// Retrieve the list of interactables that this interactor could possibly interact with this frame.
        /// </summary>
        /// <param name="validTargets">Populated List of interactables that are valid for selection or hover.</param>
        public override void GetValidTargets(List<XRBaseInteractable> validTargets)
        {
            validTargets.Clear();

            var cameraBehaviour = Camera.main;
            if (cameraBehaviour == null)
                return;

            var hFOV = GetHorizontalFOV(cameraBehaviour);

            foreach (var interactable in interactionManager.interactables)
            {
                // We can always interact with placement interactables.
                if (interactable is ARPlacementInteractable)
                    validTargets.Add(interactable);
                else if (interactable is ARBaseGestureInteractable)
                {
                    // Check if angle off of camera's forward axis is less than hFOV (more or less in camera frustum).
                    // Note: this does not take size of object into consideration.
                    // Note: this will fall down when directly over/under object (we should also check for dot
                    // product with up/down.
                    var toTarget =
                        Vector3.Normalize(interactable.transform.position - cameraBehaviour.transform.position);
                    var dotForwardToTarget = Vector3.Dot(cameraBehaviour.transform.forward, toTarget);
                    if (Mathf.Acos(dotForwardToTarget) < hFOV)
                        validTargets.Add(interactable);
                }
            }
        }
    }
}
#endif
