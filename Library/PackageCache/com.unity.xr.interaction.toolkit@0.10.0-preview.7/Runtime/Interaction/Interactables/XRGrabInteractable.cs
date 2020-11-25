using System;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Interactable component that allows basic "grab" functionality.
    /// Can attach to selecting interactor and follow it around while obeying physics (and inherit velocity when released).
    /// </summary>
    [SelectionBase]
    [DisallowMultipleComponent]
    [RequireComponent(typeof(Rigidbody))]
    [AddComponentMenu("XR/XR Grab Interactable")]
    public class XRGrabInteractable : XRBaseInteractable
    {
        const float k_DefaultTighteningAmount = 0.5f;
        const float k_DefaultSmoothingAmount = 5f;
        const float k_VelocityPredictionFactor = 0.6f;
        const float k_AngularVelocityDamping = 0.95f;
        const int k_ThrowSmoothingFrameCount = 20;
        const float k_DefaultAttachEaseInTime = 0.15f;
        const float k_DefaultThrowSmoothingDuration = 0.25f;
        const float k_DefaultThrowVelocityScale = 1.5f;
        const float k_DefaultThrowAngularVelocityScale = 1f;

        [SerializeField]
        [Tooltip("The attachment point to use on this Interactable (will use transform.position as center if none set).")]
        Transform m_AttachTransform;

        /// <summary>
        /// The attachment point to use on this Interactable (will use transform.position as center if none set).
        /// </summary>
        public Transform attachTransform
        {
            get => m_AttachTransform;
            set => m_AttachTransform = value;
        }

        [SerializeField]
        [Tooltip("Time it takes to ease in the attach (a value of 0 indicates no easing).")]
        float m_AttachEaseInTime = k_DefaultAttachEaseInTime;

        /// <summary>
        /// Time it takes to ease in the attach (a value of 0 indicates no easing).
        /// </summary>
        public float attachEaseInTime
        {
            get => m_AttachEaseInTime;
            set => m_AttachEaseInTime = value;
        }

        [SerializeField]
        [Tooltip("The movement type for the Rigidbody.")]
        MovementType m_MovementType = MovementType.Kinematic;

        /// <summary>
        /// The movement type for the <see cref="Rigidbody"/>.
        /// </summary>
        /// <seealso cref="XRBaseInteractable.MovementType"/>
        public MovementType movementType
        {
            get => m_MovementType;
            set => m_MovementType = value;
        }

        [SerializeField]
        [Tooltip("Whether this interactable should track the position of the interactor.")]
        bool m_TrackPosition = true;

        /// <summary>
        /// Whether this interactable should track the position of the interactor.
        /// </summary>
        public bool trackPosition
        {
            get => m_TrackPosition;
            set => m_TrackPosition = value;
        }

        [SerializeField]
        [Tooltip("Whether smoothing is applied to the position of the object.")]
        bool m_SmoothPosition;

        /// <summary>
        /// Whether smoothing is applied to the position of the object.
        /// </summary>
        /// <seealso cref="smoothPositionAmount"/>
        /// <seealso cref="tightenPosition"/>
        public bool smoothPosition
        {
            get => m_SmoothPosition;
            set => m_SmoothPosition = value;
        }

        [SerializeField, Range(0f, 20f)]
        [Tooltip("The smoothing applied to the object's position when following.")]
        float m_SmoothPositionAmount = k_DefaultSmoothingAmount;

        /// <summary>
        /// The smoothing applied to the object's position when following.
        /// </summary>
        /// <seealso cref="smoothPosition"/>
        /// <seealso cref="tightenPosition"/>
        public float smoothPositionAmount
        {
            get => m_SmoothPositionAmount;
            set => m_SmoothPositionAmount = value;
        }

        [SerializeField, Range(0f, 1f)]
        [Tooltip("The maximum follow position difference when using smoothing.")]
        float m_TightenPosition = k_DefaultTighteningAmount;

        /// <summary>
        /// The maximum follow position difference when using smoothing.
        /// </summary>
        /// <seealso cref="smoothPosition"/>
        /// <seealso cref="smoothPositionAmount"/>
        public float tightenPosition
        {
            get => m_TightenPosition;
            set => m_TightenPosition = value;
        }

        [SerializeField]
        [Tooltip("Whether this interactable should track the rotation of the interactor.")]
        bool m_TrackRotation = true;

        /// <summary>
        /// Whether this interactable should track the rotation of the interactor.
        /// </summary>
        public bool trackRotation
        {
            get => m_TrackRotation;
            set => m_TrackRotation = value;
        }

        [SerializeField]
        [Tooltip("Apply smoothing to the follow rotation of the object.")]
        bool m_SmoothRotation;

        /// <summary>
        /// Apply smoothing to the follow rotation of the object.
        /// </summary>
        /// <seealso cref="smoothRotationAmount"/>
        /// <seealso cref="tightenRotation"/>
        public bool smoothRotation
        {
            get => m_SmoothRotation;
            set => m_SmoothRotation = value;
        }

        [SerializeField, Range(0f, 20f)]
        [Tooltip("The smoothing applied to the object's rotation when following.")]
        float m_SmoothRotationAmount = k_DefaultSmoothingAmount;

        /// <summary>
        /// The smoothing applied to the object's rotation when following.
        /// </summary>
        /// <seealso cref="smoothRotation"/>
        /// <seealso cref="tightenRotation"/>
        public float smoothRotationAmount
        {
            get => m_SmoothRotationAmount;
            set => m_SmoothRotationAmount = value;
        }

        [SerializeField, Range(0f, 1f)]
        [Tooltip("The maximum follow rotation difference when using smoothing.")]
        float m_TightenRotation = k_DefaultTighteningAmount;

        /// <summary>
        /// The maximum follow rotation difference when using smoothing.
        /// </summary>
        /// <seealso cref="smoothRotation"/>
        /// <seealso cref="smoothRotationAmount"/>
        public float tightenRotation
        {
            get => m_TightenRotation;
            set => m_TightenRotation = value;
        }

        [SerializeField]
        [Tooltip("Whether the object inherits the interactor's velocity when released.")]
        bool m_ThrowOnDetach = true;

        /// <summary>
        /// Whether the object inherits the interactor's velocity when released.
        /// </summary>
        public bool throwOnDetach
        {
            get => m_ThrowOnDetach;
            set => m_ThrowOnDetach = value;
        }

        [SerializeField]
        [Tooltip("The time period to average thrown velocity over.")]
        float m_ThrowSmoothingDuration = k_DefaultThrowSmoothingDuration;

        /// <summary>
        /// The time period to average thrown velocity over.
        /// </summary>
        public float throwSmoothingDuration
        {
            get => m_ThrowSmoothingDuration;
            set => m_ThrowSmoothingDuration = value;
        }

        [SerializeField]
        [Tooltip("The curve to use to weight velocity smoothing (most recent frames to the right).")]
#pragma warning disable IDE0044 // Add readonly modifier -- readonly fields cannot be serialized by Unity
        AnimationCurve m_ThrowSmoothingCurve = AnimationCurve.Linear(1f, 1f, 1f, 0f);
#pragma warning restore IDE0044

        /// <summary>
        /// The curve to use to weight velocity smoothing (most recent frames to the right).
        /// </summary>
        public AnimationCurve throwSmoothingCurve
        {
            get => m_ThrowSmoothingCurve;
            set => m_ThrowSmoothingCurve = value;
        }

        [SerializeField]
        [Tooltip("The velocity scale used when throwing.")]
        float m_ThrowVelocityScale = k_DefaultThrowVelocityScale;

        /// <summary>
        /// The velocity scale used when throwing.
        /// </summary>
        public float throwVelocityScale
        {
            get => m_ThrowVelocityScale;
            set => m_ThrowVelocityScale = value;
        }

        [SerializeField]
        [Tooltip("The angular velocity scale used when throwing.")]
        float m_ThrowAngularVelocityScale = k_DefaultThrowAngularVelocityScale;

        /// <summary>
        /// The angular velocity scale used when throwing.
        /// </summary>
        public float throwAngularVelocityScale
        {
            get => m_ThrowAngularVelocityScale;
            set => m_ThrowAngularVelocityScale = value;
        }

        [SerializeField]
        [Tooltip("Whether object has gravity when released.")]
        bool m_GravityOnDetach;

        /// <summary>
        /// Whether object has gravity when released.
        /// </summary>
        public bool gravityOnDetach
        {
            get => m_GravityOnDetach;
            set => m_GravityOnDetach = value;
        }

        [SerializeField]
        [Tooltip("Whether to retain the transform's location in the scene hierarchy when the object is dropped.")]
        bool m_RetainTransformParent = true;

        /// <summary>
        /// Whether to retain the transform's location in the scene hierarchy when the object is dropped.
        /// </summary>
        public bool retainTransformParent
        {
            get => m_RetainTransformParent;
            set => m_RetainTransformParent = value;
        }

        XRBaseInteractor m_SelectingInteractor;

        /// <summary>
        /// (Read Only) The current selecting interactor for this interactable.
        /// </summary>
        public XRBaseInteractor selectingInteractor => m_SelectingInteractor;

        // Point we are attaching to on this Interactable (in interactor's attach's coordinate space)
        Vector3 m_InteractorLocalPosition;
        Quaternion m_InteractorLocalRotation;

        // Point we are moving towards each frame (eventually will be at Interactor's attach point)
        Vector3 m_TargetWorldPosition;
        Quaternion m_TargetWorldRotation;

        float m_CurrentAttachEaseTime;
        MovementType m_CurrentMovementType;

        bool m_DetachInLateUpdate;
        Vector3 m_DetachVelocity;
        Vector3 m_DetachAngularVelocity;

        int m_ThrowSmoothingCurrentFrame;
        readonly float[] m_ThrowSmoothingFrameTimes = new float[k_ThrowSmoothingFrameCount];
        readonly Vector3[] m_ThrowSmoothingVelocityFrames = new Vector3[k_ThrowSmoothingFrameCount];
        readonly Vector3[] m_ThrowSmoothingAngularVelocityFrames = new Vector3[k_ThrowSmoothingFrameCount];

        Rigidbody m_Rigidbody;
        Vector3 m_LastPosition;
        Quaternion m_LastRotation;

        // Rigidbody's previous settings
        bool m_WasKinematic;
        bool m_UsedGravity;
        Transform m_OriginalSceneParent;

        /// <inheritdoc />
        protected override void Awake()
        {
            base.Awake();

            m_CurrentMovementType = m_MovementType;
            if (m_Rigidbody == null)
                m_Rigidbody = GetComponent<Rigidbody>();
            if (m_Rigidbody == null)
                Debug.LogWarning("Grab Interactable does not have a required Rigidbody.", this);
        }

        /// <inheritdoc />
        public override void ProcessInteractable(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            switch (updatePhase)
            {
                // During Fixed update we want to perform any physics based updates (e.g., Kinematic or VelocityTracking).
                // The call to MoveToTarget will perform the correct the type of update depending on the update phase.
                case XRInteractionUpdateOrder.UpdatePhase.Fixed:
                    if (isSelected)
                    {
                        if (m_CurrentMovementType == MovementType.Kinematic)
                        {
                            PerformKinematicUpdate(updatePhase);
                        }
                        else if (m_CurrentMovementType == MovementType.VelocityTracking)
                        {
                            PerformVelocityTrackingUpdate(Time.unscaledDeltaTime, updatePhase);
                        }
                    }

                    break;

                // During Dynamic update we want to perform any GameObject based manipulation (e.g., Instantaneous).
                // The call to MoveToTarget will perform the correct the type of update depending on the update phase.
                case XRInteractionUpdateOrder.UpdatePhase.Dynamic:
                    if (isSelected)
                    {
                        UpdateTarget(Time.unscaledDeltaTime);
                        SmoothVelocityUpdate();
                        if (m_CurrentMovementType == MovementType.Instantaneous)
                        {
                            PerformInstantaneousUpdate(updatePhase);
                        }
                    }

                    break;

                // During OnBeforeUpdate we want to perform any last minute GameObject position changes before rendering (e.g., Instantaneous).
                // The call to MoveToTarget will perform the correct the type of update depending on the update phase.
                case XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender:
                    if (isSelected)
                    {
                        UpdateTarget(Time.unscaledDeltaTime);
                        if (m_CurrentMovementType == MovementType.Instantaneous)
                        {
                            PerformInstantaneousUpdate(updatePhase);
                        }
                    }

                    break;

                // Late update is only used to handle detach as late as possible.
                case XRInteractionUpdateOrder.UpdatePhase.Late:
                    if (m_DetachInLateUpdate)
                    {
                        if (m_SelectingInteractor == null)
                            Detach();
                        m_DetachInLateUpdate = false;
                    }

                    break;
            }
        }

        /// <summary>
        /// Calculate the world position to place this object at when selected.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the selection.</param>
        /// <returns>Returns the attach position in world space.</returns>
        Vector3 GetWorldAttachPosition(XRBaseInteractor interactor)
        {
            return interactor.attachTransform.position + interactor.attachTransform.rotation * m_InteractorLocalPosition;
        }

        /// <summary>
        /// Calculate the world rotation to place this object at when selected.
        /// </summary>
        /// <param name="interactor">Interactor that is initiating the selection.</param>
        /// <returns>Returns the attach rotation in world space.</returns>
        Quaternion GetWorldAttachRotation(XRBaseInteractor interactor)
        {
            return interactor.attachTransform.rotation * m_InteractorLocalRotation;
        }

        void UpdateTarget(float timeDelta)
        {
            // Compute the unsmoothed target world position and rotation
            var rawTargetWorldPosition = GetWorldAttachPosition(m_SelectingInteractor);
            var rawTargetWorldRotation = GetWorldAttachRotation(m_SelectingInteractor);

            // Apply smoothing (if configured)
            if (m_AttachEaseInTime > 0f && m_CurrentAttachEaseTime <= m_AttachEaseInTime)
            {
                var currentAttachDelta = m_CurrentAttachEaseTime / m_AttachEaseInTime;
                m_TargetWorldPosition = Vector3.Lerp(m_TargetWorldPosition, rawTargetWorldPosition, currentAttachDelta);
                m_TargetWorldRotation = Quaternion.Slerp(m_TargetWorldRotation, rawTargetWorldRotation, currentAttachDelta);
                m_CurrentAttachEaseTime += Time.unscaledDeltaTime;
            }
            else
            {
                if (m_SmoothPosition)
                {
                    m_TargetWorldPosition = Vector3.Lerp(m_TargetWorldPosition, rawTargetWorldPosition, m_SmoothPositionAmount * timeDelta);
                    m_TargetWorldPosition = Vector3.Lerp(m_TargetWorldPosition, rawTargetWorldPosition, m_TightenPosition);
                }
                else
                    m_TargetWorldPosition = rawTargetWorldPosition;

                if (m_SmoothRotation)
                {
                    m_TargetWorldRotation = Quaternion.Slerp(m_TargetWorldRotation, rawTargetWorldRotation, m_SmoothRotationAmount * timeDelta);
                    m_TargetWorldRotation = Quaternion.Slerp(m_TargetWorldRotation, rawTargetWorldRotation, m_TightenRotation);
                }
                else
                    m_TargetWorldRotation = rawTargetWorldRotation;
            }
        }

        void PerformInstantaneousUpdate(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic ||
                updatePhase == XRInteractionUpdateOrder.UpdatePhase.OnBeforeRender)
            {
                if (trackPosition)
                {
                    transform.position = m_TargetWorldPosition;
                }
                if (trackRotation)
                {
                    transform.rotation = m_TargetWorldRotation;
                }
            }
        }

        void PerformKinematicUpdate(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Fixed)
            {
                if (trackPosition)
                {
                    var positionDelta = m_TargetWorldPosition - m_Rigidbody.worldCenterOfMass;
                    m_Rigidbody.velocity = Vector3.zero;
                    m_Rigidbody.MovePosition(m_Rigidbody.position + positionDelta);
                }
                if (trackRotation)
                {
                    m_Rigidbody.angularVelocity = Vector3.zero;
                    m_Rigidbody.MoveRotation(m_TargetWorldRotation);
                }
            }
        }

        void PerformVelocityTrackingUpdate(float timeDelta, XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Fixed)
            {
                // Do velocity tracking
                if (trackPosition)
                {
                    // Scale initialized velocity by prediction factor
                    m_Rigidbody.velocity *= k_VelocityPredictionFactor;
                    var posDelta = m_TargetWorldPosition - m_Rigidbody.worldCenterOfMass;
                    var velocity = posDelta / timeDelta;

                    if (!float.IsNaN(velocity.x))
                        m_Rigidbody.velocity += velocity;
                }

                // Do angular velocity tracking
                if (trackRotation)
                {
                    // Scale initialized velocity by prediction factor
                    m_Rigidbody.angularVelocity *= k_VelocityPredictionFactor;
                    var rotationDelta = m_TargetWorldRotation * Quaternion.Inverse(m_Rigidbody.rotation);
                    rotationDelta.ToAngleAxis(out var angleInDegrees, out var rotationAxis);
                    if (angleInDegrees > 180f)
                        angleInDegrees -= 360f;

                    if (Mathf.Abs(angleInDegrees) > Mathf.Epsilon)
                    {
                        var angularVelocity = (rotationAxis * angleInDegrees * Mathf.Deg2Rad) / timeDelta;
                        if (!float.IsNaN(angularVelocity.x))
                            m_Rigidbody.angularVelocity += angularVelocity * k_AngularVelocityDamping;
                    }
                }
            }
        }

        void Detach()
        {
            if (m_ThrowOnDetach)
            {
                m_Rigidbody.velocity = m_DetachVelocity;
                m_Rigidbody.angularVelocity = m_DetachAngularVelocity;
            }
        }

        void UpdateInteractorLocalPose(XRBaseInteractor interactor)
        {
            // In order to move the Interactable to the Interactor we need to
            // calculate the Interactable attach point in the coordinate system of the
            // Interactor's attach point.
#pragma warning disable IDE0029 // Use coalesce expression -- Do not use for UnityEngine.Object types
            var attachTransform = m_AttachTransform != null ? m_AttachTransform : transform;
#pragma warning restore IDE0029
            var attachPosition = m_AttachTransform != null ? m_AttachTransform.position : transform.position;
            var attachOffset = m_Rigidbody.worldCenterOfMass - attachPosition;
            var localAttachOffset = attachTransform.InverseTransformDirection(attachOffset);

            var inverseLocalScale = interactor.attachTransform.lossyScale;
            inverseLocalScale = new Vector3(1f / inverseLocalScale.x, 1f / inverseLocalScale.y, 1f / inverseLocalScale.z);
            localAttachOffset.Scale(inverseLocalScale);

            m_InteractorLocalPosition = localAttachOffset;
            m_InteractorLocalRotation = Quaternion.Inverse(Quaternion.Inverse(m_Rigidbody.rotation) * attachTransform.rotation);
        }

        /// <inheritdoc />
        protected internal override void OnSelectEntering(XRBaseInteractor interactor)
        {
            if (interactor == null)
                return;
            base.OnSelectEntering(interactor);

            // Only do this on the first select, because otherwise the original parent will be overwritten as null.
            if (m_SelectingInteractor == null)
            {
                m_OriginalSceneParent = transform.parent;
                transform.parent = null;
            }

            m_SelectingInteractor = interactor;

            // Special case where the interactor will override this objects movement type (used for Sockets and other absolute interactors)
            m_CurrentMovementType = interactor.selectedInteractableMovementTypeOverride ?? m_MovementType;

            // Remember Rigidbody settings and setup to move
            m_WasKinematic = m_Rigidbody.isKinematic;
            m_UsedGravity = m_Rigidbody.useGravity;
            m_Rigidbody.isKinematic = (m_CurrentMovementType == MovementType.Kinematic);
            m_Rigidbody.useGravity = false;
            m_Rigidbody.drag = 0f;
            m_Rigidbody.angularDrag = 0f;

            // Forget if we have previous detach velocities
            m_DetachAngularVelocity = Vector3.zero;
            m_DetachVelocity = Vector3.zero;

            UpdateInteractorLocalPose(interactor);

            if (m_AttachEaseInTime > 0f)
            {
                m_TargetWorldPosition = m_Rigidbody.worldCenterOfMass;
                m_TargetWorldRotation = transform.rotation;
                m_CurrentAttachEaseTime = 0f;
            }

            SmoothVelocityStart();
        }

        /// <inheritdoc />
        protected internal override void OnSelectEntered(XRBaseInteractor interactor)
        {
            if (interactor == null)
                return;
            base.OnSelectEntered(interactor);
        }

        /// <inheritdoc />
        protected internal override void OnSelectExiting(XRBaseInteractor interactor)
        {
            base.OnSelectExiting(interactor);
            Drop();
        }

        /// <inheritdoc />
        protected internal override void OnSelectCanceling(XRBaseInteractor interactor)
        {
            base.OnSelectCanceling(interactor);
            Drop();
        }

        protected internal void Drop()
        {
            if(m_RetainTransformParent && m_OriginalSceneParent != null && !m_OriginalSceneParent.gameObject.activeInHierarchy)
                Debug.LogWarning("Retain Transform Parent is set to true, and has a non-null Original Scene Parent. "+
                    "However, the old parent is deactivated so we are choosing not to re-parent upon dropping.", this);
            else if (m_RetainTransformParent && gameObject.activeInHierarchy)
                transform.parent = m_OriginalSceneParent;

            // Reset Rigidbody settings
            m_Rigidbody.isKinematic = m_WasKinematic;
            m_Rigidbody.useGravity = m_UsedGravity | m_GravityOnDetach;

            m_CurrentMovementType = m_MovementType;
            m_SelectingInteractor = null;
            m_DetachInLateUpdate = true;
            SmoothVelocityEnd();
        }

        /// <inheritdoc />
        public override bool IsHoverableBy(XRBaseInteractor interactor) => true;

        void SmoothVelocityStart()
        {
            if (m_SelectingInteractor == null)
                return;
            m_LastPosition = m_SelectingInteractor.attachTransform.position;
            m_LastRotation = m_SelectingInteractor.attachTransform.rotation;
            Array.Clear(m_ThrowSmoothingFrameTimes, 0, m_ThrowSmoothingFrameTimes.Length);
            Array.Clear(m_ThrowSmoothingVelocityFrames, 0, m_ThrowSmoothingVelocityFrames.Length);
            Array.Clear(m_ThrowSmoothingAngularVelocityFrames, 0, m_ThrowSmoothingAngularVelocityFrames.Length);
            m_ThrowSmoothingCurrentFrame = 0;
        }

        void SmoothVelocityEnd()
        {
            if (m_ThrowOnDetach)
            {
                var smoothedVelocity = GetSmoothedVelocityValue(m_ThrowSmoothingVelocityFrames);
                var smoothedAngularVelocity = GetSmoothedVelocityValue(m_ThrowSmoothingAngularVelocityFrames);
                m_DetachVelocity = smoothedVelocity * m_ThrowVelocityScale;
                m_DetachAngularVelocity = smoothedAngularVelocity * m_ThrowAngularVelocityScale;
            }
        }

        void SmoothVelocityUpdate()
        {
            if (m_SelectingInteractor == null)
                return;
            m_ThrowSmoothingFrameTimes[m_ThrowSmoothingCurrentFrame] = Time.time;
            m_ThrowSmoothingVelocityFrames[m_ThrowSmoothingCurrentFrame] = (m_SelectingInteractor.attachTransform.position - m_LastPosition) / Time.deltaTime;

            var velocityDiff = (m_SelectingInteractor.attachTransform.rotation * Quaternion.Inverse(m_LastRotation));
            m_ThrowSmoothingAngularVelocityFrames[m_ThrowSmoothingCurrentFrame] = (new Vector3(Mathf.DeltaAngle(0f, velocityDiff.eulerAngles.x), Mathf.DeltaAngle(0f, velocityDiff.eulerAngles.y), Mathf.DeltaAngle(0f, velocityDiff.eulerAngles.z))
                / Time.deltaTime) * Mathf.Deg2Rad;

            m_ThrowSmoothingCurrentFrame = (m_ThrowSmoothingCurrentFrame + 1) % k_ThrowSmoothingFrameCount;
            m_LastPosition = m_SelectingInteractor.attachTransform.position;
            m_LastRotation = m_SelectingInteractor.attachTransform.rotation;
        }

        Vector3 GetSmoothedVelocityValue(Vector3[] velocityFrames)
        {
            var calcVelocity = new Vector3();
            var frameCounter = 0;
            var totalWeights = 0f;
            for (; frameCounter < k_ThrowSmoothingFrameCount; frameCounter++)
            {
                var frameIdx = (((m_ThrowSmoothingCurrentFrame - frameCounter - 1) % k_ThrowSmoothingFrameCount) + k_ThrowSmoothingFrameCount) % k_ThrowSmoothingFrameCount;
                if (m_ThrowSmoothingFrameTimes[frameIdx] == 0f)
                    break;

                var timeAlpha = (Time.time - m_ThrowSmoothingFrameTimes[frameIdx]) / m_ThrowSmoothingDuration;
                var velocityWeight = m_ThrowSmoothingCurve.Evaluate(Mathf.Clamp(1f - timeAlpha, 0f, 1f));
                calcVelocity += velocityFrames[frameIdx] * velocityWeight;
                totalWeights += velocityWeight;
                if (Time.time - m_ThrowSmoothingFrameTimes[frameIdx] > m_ThrowSmoothingDuration)
                    break;
            }

            if (totalWeights > 0f)
                return calcVelocity / totalWeights;

            return Vector3.zero;
        }
    }
}
