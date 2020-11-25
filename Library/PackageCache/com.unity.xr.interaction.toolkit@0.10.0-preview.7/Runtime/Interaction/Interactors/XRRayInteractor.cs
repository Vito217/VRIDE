using System;
using System.Collections.Generic;
using System.Linq;
using UnityEngine.EventSystems;
using UnityEngine.InputSystem;
using UnityEngine.XR.Interaction.Toolkit.UI;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Interactor used for interacting with interactables at a distance. This is handled via raycasts
    /// that update the current set of valid targets for this interactor.
    /// </summary>
    [DisallowMultipleComponent]
    [AddComponentMenu("XR/XR Ray Interactor")]
    public class XRRayInteractor : XRBaseControllerInteractor, IUIInteractor
    {
        protected class RaycastHitComparer : IComparer<RaycastHit>
        {
            public int Compare(RaycastHit a, RaycastHit b)
            {
                var aDistance = a.collider != null ? a.distance : float.MaxValue;
                var bDistance = b.collider != null ? b.distance : float.MaxValue;
                return aDistance.CompareTo(bDistance);
            }
        }

        const int k_MaxRaycastHits = 10;

        const int k_MinLineSamples = 2;
        const int k_MaxLineSamples = 100;

        /// <summary>
        /// Sets which trajectory path to use for the cast when detecting collisions.
        /// </summary>
        public enum LineType
        {
            StraightLine,
            ProjectileCurve,
            BezierCurve,
        }

        /// <summary>
        /// Sets which shape of physics cast to use for the cast when detecting collisions.
        /// </summary>
        public enum HitDetectionType
        {
            Raycast,
            SphereCast,
        }

        [SerializeField]
        LineType m_LineType = LineType.StraightLine;
        /// <summary>
        /// Gets or sets the type of ray cast.
        /// </summary>
        public LineType lineType
        {
            get => m_LineType;
            set
            {
                m_LineType = value;
                RebuildSamplePoints();
            }
        }

        [SerializeField]
        float m_MaxRaycastDistance = 30f;
        /// <summary>
        /// Gets or sets the max distance of ray cast. Increase this value will let you reach further.
        /// </summary>
        public float maxRaycastDistance
        {
            get => m_MaxRaycastDistance;
            set => m_MaxRaycastDistance = value;
        }

        [SerializeField]
        Transform m_ReferenceFrame;
        /// <summary>
        /// Gets or sets the reference frame of the projectile.
        /// If not set it will try to find the <see cref="XRRig.rig"/> object, and if that does not exist it will use its own Transform by default.
        /// </summary>
        public Transform referenceFrame
        {
            get => m_ReferenceFrame;
            set => m_ReferenceFrame = value;
        }

        [SerializeField]
        float m_Velocity = 16f;
        /// <summary>
        /// Initial velocity of the projectile. Increase this value will make the curve reach further.
        /// </summary>
        public float velocity
        {
            get => m_Velocity;
            set => m_Velocity = value;
        }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("Velocity has been deprecated. Use velocity instead. (UnityUpgradable) -> velocity")]
        public float Velocity
        {
            get => velocity;
            set => velocity = value;
        }
#pragma warning restore IDE1006

        [SerializeField]
        float m_Acceleration = 9.8f;
        /// <summary>
        /// Gravity of the projectile in the reference frame.
        /// </summary>
        public float acceleration
        {
            get => m_Acceleration;
            set => m_Acceleration = value;
        }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("Acceleration has been deprecated. Use acceleration instead. (UnityUpgradable) -> acceleration")]
        public float Acceleration
        {
            get => acceleration;
            set => acceleration = value;
        }
#pragma warning restore IDE1006

        [SerializeField]
        float m_AdditionalFlightTime = 0.5f;
        /// <summary>
        /// Additional flight time after the projectile lands at the same height of the start point in the tracking space.
        /// Increase this value will make the end point drop lower in height.
        /// </summary>
        public float additionalFlightTime
        {
            get => m_AdditionalFlightTime;
            set => m_AdditionalFlightTime = value;
        }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("AdditionalFlightTime has been deprecated. Use additionalFlightTime instead. (UnityUpgradable) -> additionalFlightTime")]
        public float AdditionalFlightTime
        {
            get => additionalFlightTime;
            set => additionalFlightTime = value;
        }
#pragma warning restore IDE1006

        [SerializeField]
        float m_EndPointDistance = 30f;
        /// <summary>
        /// Increase this value distance will make the end of curve further from the start point.
        /// </summary>
        public float endPointDistance
        {
            get => m_EndPointDistance;
            set => m_EndPointDistance = value;
        }

        [SerializeField]
        float m_EndPointHeight = -10f;
        /// <summary>
        /// Decrease this value will make the end of the curve drop lower relative to the start point.
        /// </summary>
        public float endPointHeight
        {
            get => m_EndPointHeight;
            set => m_EndPointHeight = value;
        }

        [SerializeField]
        float m_ControlPointDistance = 10f;
        /// <summary>
        /// Increase this value will make the peak of the curve further from the start point.
        /// </summary>
        public float controlPointDistance
        {
            get => m_ControlPointDistance;
            set => m_ControlPointDistance = value;
        }

        [SerializeField]
        float m_ControlPointHeight = 5f;
        /// <summary>
        /// Increase this value will make the peak of the curve higher relative to the start point.
        /// </summary>
        public float controlPointHeight
        {
            get => m_ControlPointHeight;
            set => m_ControlPointHeight = value;
        }

        [SerializeField]
        [Range(k_MinLineSamples, k_MaxLineSamples)]
        int m_SampleFrequency = 20;
        /// <summary>
        /// Gets or sets the number of sample points of the curve, should be at least 3,
        /// the higher the better quality.
        /// </summary>
        public int sampleFrequency
        {
            get => m_SampleFrequency;
            set
            {
                m_SampleFrequency = value;
                RebuildSamplePoints();
            }
        }

        [SerializeField]
        HitDetectionType m_HitDetectionType = HitDetectionType.Raycast;
        /// <summary>
        /// Sets which type of hit detection to use for the raycast.
        /// </summary>
        public HitDetectionType hitDetectionType
        {
            get => m_HitDetectionType;
            set => m_HitDetectionType = value;
        }

        [SerializeField]
        [Range(0.01f, 0.25f)]
        float m_SphereCastRadius = 0.1f;
        /// <summary>
        /// Gets or sets radius used for sphere casting. Will use regular raycasting if set to 0 or less.
        /// </summary>
        public float sphereCastRadius
        {
            get => m_SphereCastRadius;
            set => m_SphereCastRadius = value;
        }

        [SerializeField]
        LayerMask m_RaycastMask = -1;
        /// <summary>
        /// Gets or sets layer mask used for limiting raycast targets.
        /// </summary>
        public LayerMask raycastMask
        {
            get => m_RaycastMask;
            set => m_RaycastMask = value;
        }

        [SerializeField]
        QueryTriggerInteraction m_RaycastTriggerInteraction = QueryTriggerInteraction.Ignore;
        /// <summary>
        /// Gets or sets type of interaction with trigger volumes via raycast.
        /// </summary>
        public QueryTriggerInteraction raycastTriggerInteraction
        {
            get => m_RaycastTriggerInteraction;
            set => m_RaycastTriggerInteraction = value;
        }

        [SerializeField]
        bool m_KeepSelectedTargetValid = true;
        /// <summary>
        /// Whether to keep selecting the target when not pointing to it after initially selecting it.
        /// It is recommended to set this value to <see langword="true"/> for grabbing objects, <see langword="false"/> for teleportation interactables.
        /// </summary>
        public bool keepSelectedTargetValid
        {
            get => m_KeepSelectedTargetValid;
            set => m_KeepSelectedTargetValid = value;
        }


        [SerializeField]
        bool m_HoverToSelect;
        /// <summary>
        /// Gets or sets whether this uses hovering for a time period to select the interactable being hovered.
        /// </summary>
        public bool hoverToSelect
        {
            get => m_HoverToSelect;
            set => m_HoverToSelect = value;
        }

        [SerializeField]
        float m_HoverTimeToSelect = 0.5f;
        /// <summary>
        /// Gets or sets the number of seconds for which this interactor must hover over an object to select it if Hover To Select is enabled.
        /// </summary>
        public float hoverTimeToSelect
        {
            get => m_HoverTimeToSelect;
            set => m_HoverTimeToSelect = value;
        }

        [SerializeField]
        bool m_EnableUIInteraction = true;
        /// <summary>
        /// Gets or sets whether this interactor is able to affect UI.
        /// </summary>
        public bool enableUIInteraction
        {
            get => m_EnableUIInteraction;
            set
            {
                if (m_EnableUIInteraction != value)
                {
                    m_EnableUIInteraction = value;
                    RegisterOrUnregisterXRUIInputModule();
                }
            }
        }

        [SerializeField]
        bool m_AllowAnchorControl = true;
        /// <summary>
        /// Allows the user to move the attach anchor point using the joystick.
        /// </summary>
        /// <seealso cref="rotateSpeed"/>
        /// <seealso cref="translateSpeed"/>
        public bool allowAnchorControl
        {
            get => m_AllowAnchorControl;
            set
            {
                m_AllowAnchorControl = value;
                if (!value)
                {
                    attachTransform = m_OriginalAttachTransform;
                }
            }
        }

        [SerializeField]
        bool m_UseForceGrab = true;
        /// <summary>
        /// Force grab moves the object to your hand rather than interacting with it at a distance.
        /// </summary>
        public bool useForceGrab
        {
            get => m_UseForceGrab;
            set => m_UseForceGrab = value;
        }

        [SerializeField]
        float m_RotateSpeed = 180f;
        /// <summary>
        /// Speed that the anchor is rotated.
        /// </summary>
        /// <seealso cref="allowAnchorControl"/>
        /// <seealso cref="translateSpeed"/>
        public float rotateSpeed
        {
            get => m_RotateSpeed;
            set => m_RotateSpeed = value;
        }

        [SerializeField]
        float m_TranslateSpeed = 1f;
        /// <summary>
        /// Speed that the anchor is translated.
        /// </summary>
        /// <seealso cref="allowAnchorControl"/>
        /// <seealso cref="rotateSpeed"/>
        public float translateSpeed
        {
            get => m_TranslateSpeed;
            set => m_TranslateSpeed = value;
        }

        // TODO Does this really need to be public? protected probably makes more sense
        /// <summary>
        /// Gets the signed angle between the controller's forward direction and the tracking space.
        /// </summary>
        public float angle
        {
            get
            {
                var castForward = startTransform.forward;
                var projectedForward = Vector3.ProjectOnPlane(castForward, m_ReferenceFrame.up);
                return Mathf.Approximately(Vector3.Angle(castForward, projectedForward), 0f)
                    ? 0f
                    : Vector3.SignedAngle(castForward, projectedForward, Vector3.Cross(castForward, projectedForward));
            }
        }

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("Angle has been deprecated. Use angle instead. (UnityUpgradable) -> angle")]
        public float Angle => angle;
#pragma warning restore IDE1006

        readonly List<XRBaseInteractable> m_ValidTargets = new List<XRBaseInteractable>();
        /// <summary>
        /// List of interactables that this interactor could possibly interact with this frame.
        /// Populated during <see cref="ProcessInteractor"/>.
        /// </summary>
        protected override List<XRBaseInteractable> validTargets => m_ValidTargets;

        Transform m_OriginalAttachTransform;
        /// <summary>
        /// The <see cref="Transform"/> that upon entering selection
        /// (when this interactor first initiates selection of an interactable),
        /// this interactor will copy the pose of the attach <see cref="Transform"/> values into.
        /// </summary>
        /// <remarks>
        /// Automatically instantiated and set in <see cref="Awake"/>.
        /// Setting this will not automatically destroy the previous object.
        /// </remarks>
        /// <seealso cref="XRBaseInteractor.attachTransform"/>
        protected Transform originalAttachTransform
        {
            get => m_OriginalAttachTransform;
            set
            {
                // TODO Should it copy values from the previously set Transform so OnSelectExit can restore properly if this changes while selected?
                m_OriginalAttachTransform = value;
            }
        }

        /// <summary>
        /// The found <see cref="XRRig"/>. Used to determine the <see cref="referenceFrame"/> when it was not explicitly defined.
        /// </summary>
        XRRig m_XRRig;

        // reusable array of raycast hits
        readonly RaycastHit[] m_RaycastHits = new RaycastHit[k_MaxRaycastHits];
        readonly RaycastHitComparer m_RaycastHitComparer = new RaycastHitComparer();

        // reusable list of sample points
        Vector3[] m_SamplePoints;
        int m_NoSamplePoints = -1;

        // state to manage hover selection
        XRBaseInteractable m_CurrentNearestObject;
        float m_LastTimeHoveredObjectChanged;
        bool m_PassedHoverTimeToSelect;

        int m_HitCount;
        int m_HitPositionInLine = -1;

        /// <summary>
        /// Control points to calculate the bezier curve.
        /// </summary>
        readonly Vector3[] m_ControlPoints = new Vector3[3];

        /// <summary>
        /// The starting transform of any Raycasts. Uses the Original Attach transform, falling back to this transform.
        /// </summary>
#pragma warning disable IDE0029 // Use coalesce expression -- Do not use for UnityEngine.Object types
        Transform startTransform => m_OriginalAttachTransform != null ? m_OriginalAttachTransform : transform;
#pragma warning restore IDE0029

        // Input Module for fast access to UI systems.
        XRUIInputModule m_InputModule;

        XRUIInputModule m_RegisteredInputModule;

        // Used by UpdateUIModel to retrieve the line points to pass along to Unity UI.
        static Vector3[] s_CachedLinePoints;

        protected void OnValidate()
        {
            RegisterOrUnregisterXRUIInputModule();
        }

        /// <inheritdoc />
        protected override void Awake()
        {
            base.Awake();

            if (m_XRRig == null)
            {
                m_XRRig = FindObjectOfType<XRRig>();
                if (m_XRRig == null)
                {
                    Debug.LogWarning("No XR Rig could be found", this);
                }
            }

            if (m_OriginalAttachTransform == null)
            {
                var originalAttach = new GameObject($"[{gameObject.name}] Original Attach");
                m_OriginalAttachTransform = originalAttach.transform;
                m_OriginalAttachTransform.SetParent(transform);
                m_OriginalAttachTransform.localPosition = Vector3.zero;
                m_OriginalAttachTransform.localRotation = Quaternion.identity;
            }
        }

        /// <inheritdoc />
        protected override void OnEnable()
        {
            base.OnEnable();

            RebuildSamplePoints();
            FindReferenceFrame();

            if (m_EnableUIInteraction)
                RegisterWithXRUIInputModule();
        }

        /// <inheritdoc />
        protected override void OnDisable()
        {
            base.OnDisable();

            // Clear lines
            m_NoSamplePoints = -1;

            if (m_EnableUIInteraction)
                UnregisterFromXRUIInputModule();
        }

        void RebuildSamplePoints()
        {
            var samplePointsSize = m_LineType == LineType.StraightLine ? 2 : m_SampleFrequency;
            if (m_SamplePoints == null || m_SamplePoints.Length != samplePointsSize)
                m_SamplePoints = new Vector3[samplePointsSize];
            m_NoSamplePoints = 0;
        }

        void FindOrCreateXRUIInputModule()
        {
            var eventSystem = FindObjectOfType<EventSystem>();
            if (eventSystem == null)
                eventSystem = new GameObject("EventSystem", typeof(EventSystem)).GetComponent<EventSystem>();
            else
            {
                // Remove the Standalone Input Module if already implemented, since it will block the XRUIInputModule
                var standaloneInputModule = eventSystem.GetComponent<StandaloneInputModule>();
                if (standaloneInputModule != null)
                    Destroy(standaloneInputModule);
            }

            m_InputModule = eventSystem.GetComponent<XRUIInputModule>();
            if (m_InputModule == null)
                m_InputModule = eventSystem.gameObject.AddComponent<XRUIInputModule>();
        }

        /// <summary>
        /// Register with the <see cref="XRUIInputModule"/> (if necessary).
        /// </summary>
        /// <seealso cref="UnregisterFromXRUIInputModule"/>
        void RegisterWithXRUIInputModule()
        {
            if (m_InputModule == null)
                FindOrCreateXRUIInputModule();

            if (m_RegisteredInputModule == m_InputModule)
                return;

            UnregisterFromXRUIInputModule();

            m_InputModule.RegisterInteractor(this);
            m_RegisteredInputModule = m_InputModule;
        }

        /// <summary>
        /// Unregister from the <see cref="XRUIInputModule"/> (if necessary).
        /// </summary>
        /// <seealso cref="RegisterWithXRUIInputModule"/>
        void UnregisterFromXRUIInputModule()
        {
            if (m_RegisteredInputModule != null)
                m_RegisteredInputModule.UnregisterInteractor(this);

            m_RegisteredInputModule = null;
        }

        /// <summary>
        /// Register with or unregister from the Input Module (if necessary).
        /// </summary>
        /// <remarks>
        /// If this behavior is not active and enabled, this function does nothing.
        /// </remarks>
        void RegisterOrUnregisterXRUIInputModule()
        {
            if (!isActiveAndEnabled || !Application.isPlaying)
                return;

            if (m_EnableUIInteraction)
                RegisterWithXRUIInputModule();
            else
                UnregisterFromXRUIInputModule();
        }

        /// <summary>
        /// (Obsolete) Use <see cref="ILineRenderable.GetLinePoints"/> instead.
        /// </summary>
        /// <param name="linePoints">Obsolete.</param>
        /// <param name="numPoints">Obsolete.</param>
        /// <param name="_">Dummy value to support old function signature.</param>
        /// <returns>Obsolete.</returns>
        [Obsolete("GetLinePoints with ref int parameter has been deprecated. Use signature with out int parameter instead.")]
        // ReSharper disable RedundantAssignment
        public bool GetLinePoints(ref Vector3[] linePoints, ref int numPoints, int _ = default)
            // ReSharper restore RedundantAssignment
        {
            return GetLinePoints(ref linePoints, out numPoints);
        }

        /// <inheritdoc />
        public bool GetLinePoints(ref Vector3[] linePoints, out int numPoints)
        {
            if (m_SamplePoints == null || m_SamplePoints.Length < 2 || m_NoSamplePoints < 2)
            {
                numPoints = default;
                return false;
            }

            if (linePoints == null || linePoints.Length != m_NoSamplePoints)
            {
                linePoints = new Vector3[m_NoSamplePoints];
            }

            // Transform samples points from local to world space
            for (var i = 0; i < m_SamplePoints.Length; ++i)
            {
                linePoints[i] = startTransform.TransformPoint(m_SamplePoints[i]);
            }

            numPoints = m_NoSamplePoints;
            return true;
        }

        /// <summary>
        /// (Obsolete) Use <see cref="ILineRenderable.TryGetHitInfo"/> instead.
        /// </summary>
        /// <param name="position">Obsolete.</param>
        /// <param name="normal">Obsolete.</param>
        /// <param name="positionInLine">Obsolete.</param>
        /// <param name="isValidTarget">Obsolete.</param>
        /// <param name="_">Dummy value to support old function signature.</param>
        /// <returns>Obsolete.</returns>
        [Obsolete("TryGetHitInfo with ref parameters has been deprecated. Use signature with out parameters instead.")]
        // ReSharper disable RedundantAssignment
        public bool TryGetHitInfo(ref Vector3 position, ref Vector3 normal, ref int positionInLine, ref bool isValidTarget, int _ = default)
            // ReSharper restore RedundantAssignment
        {
            return TryGetHitInfo(out position, out normal, out positionInLine, out isValidTarget);
        }

        /// <inheritdoc />
        public bool TryGetHitInfo(out Vector3 position, out Vector3 normal, out int positionInLine, out bool isValidTarget)
        {
            position = default;
            normal = default;
            positionInLine = default;
            isValidTarget = false;
            var isValidRaycast = false;

            var distance = float.MaxValue;
            var rayIndex = int.MaxValue;

            if (GetCurrentRaycastHit(out var raycastHit))
            {
                position = raycastHit.point;
                normal = raycastHit.normal;
                rayIndex = m_HitPositionInLine;
                positionInLine = m_HitPositionInLine;
                distance = raycastHit.distance;

                isValidRaycast = true;

                // Determine if the collider is registered as an interactable and the interactable is being hovered
                var interactable = interactionManager.TryGetInteractableForCollider(raycastHit.collider);
                isValidTarget = interactable != null && hoverTargets.Contains(interactable);
            }

            if (GetCurrentUIRaycastResult(out var result, out var raycastPointIndex))
            {
                if (raycastPointIndex >= 0 && (raycastPointIndex < rayIndex || raycastPointIndex == rayIndex && result.distance <= distance))
                {
                    position = result.worldPosition;
                    normal = result.worldNormal;
                    positionInLine = raycastPointIndex;

                    isValidTarget = result.gameObject != null;
                }

                isValidRaycast = true;
            }

            return isValidRaycast;
        }

        /// <inheritdoc />
        public void UpdateUIModel(ref TrackedDeviceModel model)
        {
            model.position = startTransform.position;
            model.orientation = startTransform.rotation;
            model.select = isUISelectActive;

            GetLinePoints(ref s_CachedLinePoints, out var numPoints);

            var raycastPoints = model.raycastPoints;
            raycastPoints.Clear();
            if (numPoints > 0 && s_CachedLinePoints != null)
            {
                raycastPoints.Capacity = raycastPoints.Count + numPoints;
                for (var i = 0; i < numPoints; ++i)
                    raycastPoints.Add(s_CachedLinePoints[i]);
            }
            model.raycastLayerMask = raycastMask;
        }

        /// <inheritdoc />
        public bool TryGetUIModel(out TrackedDeviceModel model)
        {
            if (m_InputModule != null)
            {
                return m_InputModule.GetTrackedDeviceModel(this, out model);
            }

            model = new TrackedDeviceModel(-1);
            return false;
        }

        /// <summary>
        /// This function will return the first raycast result, if any raycast results are available.
        /// </summary>
        /// <param name="raycastHit">When this method returns, contains the raycast result if available; otherwise, the default value.</param>
        /// <returns>Returns <see langword="true"/> if the <paramref name="raycastHit"/> parameter contains a valid raycast result.
        /// Returns <see langword="false"/> otherwise.</returns>
        public bool GetCurrentRaycastHit(out RaycastHit raycastHit)
        {
            if (m_HitCount > 0 && m_RaycastHits.Length > 0)
            {
                raycastHit = m_RaycastHits[0];
                return true;
            }

            raycastHit = default;
            return false;
        }

        bool GetCurrentUIRaycastResult(out RaycastResult result, out int raycastPointIndex)
        {
            if (TryGetUIModel(out var model))
            {
                result = model.implementationData.lastFrameRaycast;
                raycastPointIndex = model.implementationData.lastFrameRaycastResultPositionInLine;
                return result.isValid;
            }

            result = default;
            raycastPointIndex = -1;
            return false;
        }

        void UpdateBezierControlPoints()
        {
            var forward = startTransform.forward;
            var up = m_ReferenceFrame.up;
            m_ControlPoints[0] = startTransform.position;
            m_ControlPoints[1] = m_ControlPoints[0] + forward * m_ControlPointDistance + up * m_ControlPointHeight;
            m_ControlPoints[2] = m_ControlPoints[0] + forward * m_EndPointDistance + up * m_EndPointHeight;
        }

        static Vector3 CalculateBezierPoint(float t, Vector3 start, Vector3 control, Vector3 end)
        {
            return Mathf.Pow(1f - t, 2f) * start + 2f * (1f - t) * t * control + Mathf.Pow(t, 2f) * end;
        }

        static Vector3 CalculateProjectilePoint(float t, Vector3 start, Vector3 velocity, Vector3 acceleration)
        {
            return start + velocity * t + 0.5f * acceleration * t * t;
        }

        void FindReferenceFrame()
        {
            if (m_ReferenceFrame != null)
                return;

            if (m_XRRig != null)
            {
                var rig = m_XRRig.rig;
                if (rig != null)
                {
                    m_ReferenceFrame = rig.transform;
                }
                else
                {
                    m_ReferenceFrame = transform;
                    Debug.Log($"Reference frame of the projectile curve not set and {nameof(XRRig)}.{nameof(XRRig.rig)} is not set, using self as default.", this);
                }
            }
            else
            {
                m_ReferenceFrame = transform;
                Debug.Log($"Reference frame of the projectile curve not set and {nameof(XRRig)} is not found, using self as default.", this);
            }
        }

        static bool TryRead2DAxis(InputAction action, out Vector2 output)
        {
            if (action != null)
            {
                output = action.ReadValue<Vector2>();
                return true;
            }
            output = default;
            return false;
        }

        protected virtual void RotateAnchor(Transform anchor, float directionAmount)
        {
            var axis = m_XRRig != null && m_XRRig.rig != null
                ? m_XRRig.rig.transform.up // TODO Why not just use m_ReferenceFrame.up?
                : attachTransform.transform.up; // TODO Why not just use anchor.up?
            anchor.Rotate(axis, directionAmount * (m_RotateSpeed * Time.deltaTime));
        }

        protected virtual void TranslateAnchor(Transform originalAnchor, Transform anchor, float directionAmount)
        {
            var originalAnchorPosition = originalAnchor.position;
            var originalAnchorForward = originalAnchor.forward;

            var resultingPosition = attachTransform.position + originalAnchorForward * (directionAmount * m_TranslateSpeed * Time.deltaTime);

            // Check the delta between the original position, and the calculated position. stop the new position
            var posInAttachSpace = resultingPosition - originalAnchorPosition;
            var dotResult = Vector3.Dot(posInAttachSpace, originalAnchorForward);

            attachTransform.position = dotResult > 0f ? resultingPosition : originalAnchorPosition;
        }

        /// <inheritdoc />
        public override void ProcessInteractor(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            base.ProcessInteractor(updatePhase);

            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic)
            {
                if (selectTarget && m_AllowAnchorControl)
                {
                    var ctrl = xrController as XRController;
                    if (ctrl != null && ctrl.inputDevice.isValid)
                    {
                        ctrl.inputDevice.IsPressed(ctrl.rotateObjectLeft, out var leftPressed, ctrl.axisToPressThreshold);
                        ctrl.inputDevice.IsPressed(ctrl.rotateObjectRight, out var rightPressed, ctrl.axisToPressThreshold);

                        ctrl.inputDevice.IsPressed(ctrl.moveObjectIn, out var inPressed, ctrl.axisToPressThreshold);
                        ctrl.inputDevice.IsPressed(ctrl.moveObjectOut, out var outPressed, ctrl.axisToPressThreshold);

                        if (inPressed || outPressed)
                        {
                            var directionAmount = inPressed ? 1f : -1f;
                            TranslateAnchor(m_OriginalAttachTransform, attachTransform, directionAmount);
                        }
                        if (leftPressed || rightPressed)
                        {
                            var directionAmount = leftPressed ? -1f : 1f;
                            RotateAnchor(attachTransform, directionAmount);
                        }
                    }

                    var actionBasedController = xrController as ActionBasedController;
                    if (actionBasedController != null)
                    {
                        if (TryRead2DAxis(actionBasedController.rotateAnchorAction.action, out var rotateAmt))
                        {
                            if (Math.Abs(rotateAmt.x) > actionBasedController.anchorControlDeadzone &&
                                Math.Abs(rotateAmt.y) < actionBasedController.anchorControlOffAxisDeadzone)
                            {
                                RotateAnchor(attachTransform, rotateAmt.x);
                            }
                        }

                        if (TryRead2DAxis(actionBasedController.translateAnchorAction.action, out var translateAmt))
                        {
                            if (Math.Abs(translateAmt.y) > actionBasedController.anchorControlDeadzone &&
                                Math.Abs(translateAmt.x) < actionBasedController.anchorControlOffAxisDeadzone)
                            {
                                TranslateAnchor(m_OriginalAttachTransform, attachTransform, translateAmt.y);
                            }
                        }
                    }
                }

                // Check to see if we have a new hover object
                GetValidTargets(m_ValidTargets);
                var nearestObject = m_ValidTargets.FirstOrDefault();
                if (nearestObject != m_CurrentNearestObject)
                {
                    m_CurrentNearestObject = nearestObject;
                    m_LastTimeHoveredObjectChanged = Time.time;
                    m_PassedHoverTimeToSelect = false;
                }
                else if (nearestObject && !m_PassedHoverTimeToSelect)
                {
                    var progressToHoverSelect = Mathf.Clamp01((Time.time - m_LastTimeHoveredObjectChanged) / m_HoverTimeToSelect);
                    if (progressToHoverSelect >= 1f)
                        m_PassedHoverTimeToSelect = true;
                }
            }
        }

        int CheckCollidersBetweenPoints(Vector3 from, Vector3 to)
        {
            Array.Clear(m_RaycastHits, 0, k_MaxRaycastHits);

            // Cast from last point to next point to check if there are hits in between
            if (m_HitDetectionType == HitDetectionType.SphereCast && m_SphereCastRadius > 0f)
            {
                return Physics.SphereCastNonAlloc(from, m_SphereCastRadius, (to - from).normalized,
                    m_RaycastHits, Vector3.Distance(to, from), raycastMask, raycastTriggerInteraction);
            }

            return Physics.RaycastNonAlloc(from, (to - from).normalized,
                m_RaycastHits, Vector3.Distance(to, from), raycastMask, raycastTriggerInteraction);
        }

        /// <summary>
        /// Retrieve the list of interactables that this interactor could possibly interact with this frame.
        /// This list is sorted by priority (in this case distance).
        /// </summary>
        /// <param name="validTargets">Populated List of interactables that are valid for selection or hover.</param>
        public override void GetValidTargets(List<XRBaseInteractable> validTargets)
        {
            validTargets.Clear();

            // If we haven't initialized cleanly, bail out
            if (m_SamplePoints == null || m_SamplePoints.Length < 2)
            {
                return;
            }

            m_NoSamplePoints = 1;
            m_SamplePoints[0] = startTransform.position;

            // Pointers used to sample the curve and check colliders between points
            Vector3 previousPoint = m_SamplePoints[0];
            Vector3 nextPoint;

            m_HitCount = 0;
            m_HitPositionInLine = 0;
            var accumulatedHits = 0;
            int maxSamplePoints;

            switch (m_LineType)
            {
                case LineType.StraightLine:
                    nextPoint = previousPoint + startTransform.forward * maxRaycastDistance;
                    m_HitCount = CheckCollidersBetweenPoints(previousPoint, nextPoint);

                    if (m_HitCount != 0)
                        m_HitPositionInLine = 1; // hit position is between point 0 and point 1

                    // Save the "virtual" end point of the line
                    m_SamplePoints[m_NoSamplePoints] = nextPoint;
                    m_NoSamplePoints++;
                    break;

                case LineType.ProjectileCurve:
                    var flightTime = 2f * m_Velocity * Mathf.Sin(Mathf.Abs(angle) * Mathf.Deg2Rad) / m_Acceleration + m_AdditionalFlightTime;
                    var velocityVector = startTransform.forward * m_Velocity;
                    var accelerationVector = m_ReferenceFrame.up * -1f * m_Acceleration;

                    maxSamplePoints = m_SamplePoints.Length;
                    accumulatedHits = 0;
                    for (var i = 1; i < m_SampleFrequency && m_NoSamplePoints < maxSamplePoints; ++i)
                    {
                        var t = i / (float)(m_SampleFrequency - 1) * flightTime;

                        nextPoint = CalculateProjectilePoint(t, startTransform.position, velocityVector, accelerationVector);

                        // Check collider only when there has not been a hit point
                        if (accumulatedHits == 0)
                        {
                            accumulatedHits += CheckCollidersBetweenPoints(previousPoint, nextPoint);
                            if (accumulatedHits != 0)
                                m_HitPositionInLine = i;
                        }

                        // Keep sampling
                        m_SamplePoints[m_NoSamplePoints] = nextPoint;
                        m_NoSamplePoints++;
                        previousPoint = nextPoint;
                    }
                    m_HitCount = accumulatedHits;

                    break;

                case LineType.BezierCurve:
                    // Update control points for bezier curve
                    UpdateBezierControlPoints();

                    maxSamplePoints = m_SamplePoints.Length;

                    for (var i = 1; i < m_SampleFrequency && m_NoSamplePoints < maxSamplePoints; ++i)
                    {
                        var t = i / (float)(m_SampleFrequency - 1);
                        nextPoint = CalculateBezierPoint(t, m_ControlPoints[0], m_ControlPoints[1], m_ControlPoints[2]);

                        // Check collider only when there has not been a hit point
                        if (accumulatedHits == 0)
                        {
                            accumulatedHits += CheckCollidersBetweenPoints(previousPoint, nextPoint);
                            if (accumulatedHits != 0)
                                m_HitPositionInLine = i;
                        }

                        // Keep sampling
                        m_SamplePoints[m_NoSamplePoints] = nextPoint;
                        m_NoSamplePoints++;
                        previousPoint = nextPoint;
                    }

                    m_HitCount = accumulatedHits;
                    break;
            }

            // Save sample points as the local points of the startTransform,
            // when accessing the sample points at a different time they will have to be transformed into world space.
            for (var i = 0; i < m_SamplePoints.Length; ++i)
            {
                m_SamplePoints[i] = startTransform.InverseTransformPoint(m_SamplePoints[i]);
            }

            // Sort all the hits by distance to the controller
            if (m_HitCount > 0)
            {
                SortingHelpers.Sort(m_RaycastHits, m_RaycastHitComparer);
                for (var i = 0; i < Math.Min(m_HitCount, k_MaxRaycastHits); ++i)
                {
                    var interactable = interactionManager.TryGetInteractableForCollider(m_RaycastHits[i].collider);
                    if (interactable != null && !validTargets.Contains(interactable))
                        validTargets.Add(interactable);
                    else
                        break;
                }
            }
        }

        /// <inheritdoc />
        public override bool CanHover(XRBaseInteractable interactable)
        {
            return base.CanHover(interactable) && (selectTarget == null || selectTarget == interactable);
        }

        /// <inheritdoc />
        public override bool CanSelect(XRBaseInteractable interactable)
        {
            var selectActivated = (hoverToSelect && (m_CurrentNearestObject == interactable) && m_PassedHoverTimeToSelect) || base.CanSelect(interactable);

            // Check if selectTarget is a valid target or if we enable sticky select to fake selectTarget as valid when we selected it but are not pointing at it.
            return selectActivated &&
                   (selectTarget == null || (selectTarget == interactable && (keepSelectedTargetValid || m_ValidTargets.Contains(interactable))));
        }

        /// <inheritdoc />
        protected internal override void OnSelectExiting(XRBaseInteractable interactable)
        {
            base.OnSelectExiting(interactable);

            attachTransform.position = m_OriginalAttachTransform.position;
            attachTransform.rotation = m_OriginalAttachTransform.rotation;
            attachTransform.localScale = m_OriginalAttachTransform.localScale;
        }

        protected internal override void OnSelectEntering(XRBaseInteractable interactable)
        {
            base.OnSelectEntering(interactable);

            m_OriginalAttachTransform.position = attachTransform.position;
            m_OriginalAttachTransform.rotation = attachTransform.rotation;
            m_OriginalAttachTransform.localScale = attachTransform.localScale;

            if (!m_UseForceGrab)
            {
                if (GetCurrentRaycastHit(out var raycastHit))
                {
                    attachTransform.position = raycastHit.point;
                }
            }
        }
    }
}
