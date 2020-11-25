using System;
using System.Collections;
using System.Collections.Generic;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The XR Rig component is typically attached to the base object of the XR Rig,
    /// and stores the <see cref="GameObject"/> that will be manipulated via locomotion.
    /// It is also used for offsetting the camera.
    /// </summary>
    [AddComponentMenu("XR/XR Rig")]
    [DisallowMultipleComponent]
    public class XRRig : MonoBehaviour
    {
        const float k_DefaultCameraYOffset = 1.36144f;

        [SerializeField]
        [Tooltip("The \"Rig\" GameObject is used to refer to the base of the XR Rig, by default it is this GameObject." +
            " This is the GameObject that will be manipulated via locomotion.")]
        GameObject m_RigBaseGameObject;

        /// <summary>
        /// The "Rig" <see cref="GameObject"/> is used to refer to the base of the XR Rig, by default it is this <see cref="GameObject"/>.
        /// This is the <see cref="GameObject"/> that will be manipulated via locomotion.
        /// </summary>
        public GameObject rig
        {
            get => m_RigBaseGameObject;
            set => m_RigBaseGameObject = value;
        }

        [SerializeField]
        [Tooltip("The GameObject to move to desired height off the floor (defaults to this object if none provided).")]
        GameObject m_CameraFloorOffsetObject;

        /// <summary>
        /// The <see cref="GameObject"/> to move to desired height off the floor (defaults to this object if none provided).
        /// </summary>
        public GameObject cameraFloorOffsetObject
        {
            get => m_CameraFloorOffsetObject;
            set
            {
                m_CameraFloorOffsetObject = value;
                SetupCamera();
            }
        }

        [SerializeField]
        [Tooltip("The GameObject that contains the camera, this is usually the \"Head\" of XR rigs.")]
        GameObject m_CameraGameObject;

        /// <summary>
        /// The <see cref="GameObject"/> that contains the camera, this is usually the "Head" of XR rigs.
        /// </summary>
        public GameObject cameraGameObject
        {
            get => m_CameraGameObject;
            set => m_CameraGameObject = value;
        }

#if UNITY_2019_3_OR_NEWER
        [SerializeField]
        [Tooltip("The type of tracking origin to use for this Rig. Tracking origins identify where (0, 0, 0) is in the world of tracking.")]
        TrackingOriginModeFlags m_TrackingOriginMode = TrackingOriginModeFlags.Unknown;

        /// <summary>
        /// The type of tracking origin to use for this Rig. Tracking origins identify where (0, 0, 0) is in the world
        /// of tracking. Not all devices support all tracking spaces; if the selected tracking space is not set it will
        /// fall back to <see cref="TrackingSpaceType.Stationary"/>.
        /// </summary>
        public TrackingOriginModeFlags trackingOriginMode
        {
            get => m_TrackingOriginMode;
            set
            {
                m_TrackingOriginMode = value;
                TryInitializeCamera();
            }
        }
#endif // UNITY_2019_3_OR_NEWER

#pragma warning disable 0618 // Disable Obsolete warnings for TrackingSpaceType, explicitly to read in old data and upgrade.
        [SerializeField]
        [Tooltip("Set if the XR experience is Room Scale or Stationary.")]
        TrackingSpaceType m_TrackingSpace = TrackingSpaceType.Stationary;

        /// <summary>
        /// Whether the experience is rooms scale or stationary. Not all devices support all tracking spaces; if the
        /// selected tracking space is not set it will fall back to Stationary.
        /// </summary>
#if UNITY_2019_3_OR_NEWER
        [Obsolete("XRRig.trackingSpace is obsolete.  Please use XRRig.trackingOriginMode.")]
#endif
        public TrackingSpaceType trackingSpace
        {
            get => m_TrackingSpace;
            set
            {
                m_TrackingSpace = value;
                TryInitializeCamera();
            }
        }
#pragma warning restore 0618

        [SerializeField]
#if UNITY_2019_3_OR_NEWER
        [Tooltip("Camera Height to be used when in \"Device\" tracking origin mode to define the height of the user from the floor.")]
#else
        [Tooltip("Camera Height to be used when in \"Stationary\" tracking space to define the height of the user from the floor.")]
#endif
        float m_CameraYOffset = k_DefaultCameraYOffset;


        /// <summary>
        /// The amount the camera is offset from the floor (by moving the camera offset object).
        /// </summary>
        public float cameraYOffset
        {
            get => m_CameraYOffset;
            set
            {
                m_CameraYOffset = value;
                TryInitializeCamera();
            }
        }

        /// <summary>
        /// (Read Only) The rig's local position in camera space.
        /// </summary>
        public Vector3 rigInCameraSpacePos => m_CameraGameObject.transform.InverseTransformPoint(m_RigBaseGameObject.transform.position);

        /// <summary>
        /// (Read Only) The camera's local position in rig space.
        /// </summary>
        public Vector3 cameraInRigSpacePos => m_RigBaseGameObject.transform.InverseTransformPoint(m_CameraGameObject.transform.position);

        /// <summary>
        /// (Read Only) The camera's height relative to the rig.
        /// </summary>
        public float cameraInRigSpaceHeight => cameraInRigSpacePos.y;

        /// <summary>
        /// Used to cache the input subsystems without creating additional GC allocations.
        /// </summary>
        static readonly List<XRInputSubsystem> s_InputSubsystems = new List<XRInputSubsystem>();

        // Bookkeeping to track lazy initialization of the tracking space type.
        bool m_CameraInitialized;
        bool m_CameraInitializing;

        protected void OnValidate()
        {
            UpgradeTrackingSpaceToTrackingOriginMode();

            if (m_RigBaseGameObject == null)
                m_RigBaseGameObject = gameObject;

            TryInitializeCamera();
        }

        protected void Awake()
        {
            if (m_CameraFloorOffsetObject == null)
            {
                Debug.LogWarning("No Camera Floor Offset Object specified for XR Rig, using attached GameObject.", this);
                m_CameraFloorOffsetObject = gameObject;
            }
            if (m_CameraGameObject == null)
            {
                Debug.LogWarning("No Camera GameObject specified for XR Rig, using the MainCamera GameObject if it is not null.", this);
                var mainCamera = Camera.main;
                if (mainCamera != null)
                    m_CameraGameObject = mainCamera.gameObject;
                else
                    Debug.LogWarning("No Main Camera is found for XR Rig, please assign the Camera GameObject field manually.", this);
            }
        }

        protected void Start()
        {
            TryInitializeCamera();
        }

        protected virtual void OnDrawGizmos()
        {
            if (m_RigBaseGameObject != null)
            {
                // Draw XR Rig box
                Gizmos.color = Color.green;
                GizmoHelpers.DrawWireCubeOriented(m_RigBaseGameObject.transform.position, m_RigBaseGameObject.transform.rotation, 3f);
                GizmoHelpers.DrawAxisArrows(m_RigBaseGameObject.transform, 0.5f);
            }

            if (m_CameraFloorOffsetObject != null)
            {
                GizmoHelpers.DrawAxisArrows(m_CameraFloorOffsetObject.transform, 0.5f);
            }

            if (m_CameraGameObject != null)
            {
                var cameraPosition = m_CameraGameObject.transform.position;
                Gizmos.color = Color.red;
                GizmoHelpers.DrawWireCubeOriented(cameraPosition, m_CameraGameObject.transform.rotation, 0.1f);
                GizmoHelpers.DrawAxisArrows(m_CameraGameObject.transform, 0.5f);

                var floorPos = cameraPosition;
                floorPos.y = m_RigBaseGameObject.transform.position.y;
                Gizmos.DrawLine(floorPos, cameraPosition);
            }
        }

        /// <summary>
        /// Utility helper to migrate from <see cref="TrackingSpaceType"/> to <see cref="TrackingOriginModeFlags"/> seamlessly.
        /// </summary>
        void UpgradeTrackingSpaceToTrackingOriginMode()
        {
#if UNITY_2019_3_OR_NEWER
#pragma warning disable 0618 // Disable Obsolete warnings for TrackingSpaceType, explicitly to allow a proper upgrade path.
            if (m_TrackingOriginMode == TrackingOriginModeFlags.Unknown && m_TrackingSpace <= TrackingSpaceType.RoomScale)
            {
                switch (m_TrackingSpace)
                {
                    case TrackingSpaceType.RoomScale:
                        m_TrackingOriginMode = TrackingOriginModeFlags.Floor;
                        break;
                    case TrackingSpaceType.Stationary:
                        m_TrackingOriginMode = TrackingOriginModeFlags.Device;
                        break;
                }

                // Set to an invalid value to indicate the value has been migrated.
                m_TrackingSpace = (TrackingSpaceType)3;
#if UNITY_EDITOR
                EditorUtility.SetDirty(this);
#endif // UNITY_EDITOR
#pragma warning restore 0618
            }
#endif // UNITY_2019_3_OR_NEWER
        }

        void TryInitializeCamera()
        {
            m_CameraInitialized = SetupCamera();
            if (!m_CameraInitialized & !m_CameraInitializing)
                StartCoroutine(RepeatInitializeCamera());
        }

        /// <summary>
        /// Repeatedly attempt to initialize the camera.
        /// </summary>
        IEnumerator RepeatInitializeCamera()
        {
            m_CameraInitializing = true;
            yield return null;
            while (!m_CameraInitialized)
            {
                m_CameraInitialized = SetupCamera();
                yield return null;
            }
            m_CameraInitializing = false;
        }

        /// <summary>
        /// Handles re-centering and off-setting the camera in space depending on which tracking space it is setup in.
        /// </summary>
#if UNITY_2019_3_OR_NEWER
        bool SetupCamera()
        {
            SubsystemManager.GetInstances(s_InputSubsystems);

            var initialized = true;
            if (s_InputSubsystems.Count > 0)
            {
                foreach (var inputSubsystem in s_InputSubsystems)
                {
                    initialized &= SetupCamera(inputSubsystem);
                }
            }
            else
            {
#pragma warning disable 0618 // Disable Obsolete warnings for TrackingSpaceType, explicitly to allow a proper upgrade path.
                // TODO Enum type name ends with Flags but checking for value instead of HasFlags?
                // TODO Enum does not have Flags attribute, but has values like it should?
                if (m_TrackingOriginMode == TrackingOriginModeFlags.Floor)
                {
                    SetupCameraLegacy(TrackingSpaceType.RoomScale);
                }
                // TODO Unreachable code, condition is same as above
                else if (m_TrackingOriginMode == TrackingOriginModeFlags.Floor)
                {
                    SetupCameraLegacy(TrackingSpaceType.Stationary);
                }
#pragma warning restore 0618
            }

            return initialized;
        }

        bool SetupCamera(XRInputSubsystem subsystem)
        {
            if (subsystem == null)
                return false;

            var trackingSettingsSet = false;

            var desiredOffset = m_CameraYOffset;
            if (m_TrackingOriginMode == TrackingOriginModeFlags.Floor)
            {
                var supportedModes = subsystem.GetSupportedTrackingOriginModes();
                // We need to check for Unknown because we may not be in a state where we can read this data yet.
                // TODO TrackingOriginModeFlags.Unknown has a value of 0, so the warning would be triggered anyway?
                if ((supportedModes & (TrackingOriginModeFlags.Floor | TrackingOriginModeFlags.Unknown)) == 0)
                {
                    Debug.LogWarning("Attempting to set the tracking space to Room, but that is not supported by the SDK." +
                        $" Supported types: {supportedModes}.", this);
                    return true;
                }

                if (subsystem.TrySetTrackingOriginMode(m_TrackingOriginMode))
                {
                    desiredOffset = 0f;
                    trackingSettingsSet = true;
                }
            }

            if (m_TrackingOriginMode == TrackingOriginModeFlags.Device)
            {
                var supportedModes = subsystem.GetSupportedTrackingOriginModes();
                // We need to check for Unknown because we may not be in a state where we can read this data yet.
                // TODO TrackingOriginModeFlags.Unknown has a value of 0, so the warning would be triggered anyway?
                if ((supportedModes & (TrackingOriginModeFlags.Device | TrackingOriginModeFlags.Unknown)) == 0)
                {
                    Debug.LogWarning("Attempting to set the tracking space to Stationary, but that is not supported by the SDK." +
                        $" Supported types: {supportedModes}.", this);
                    return true;
                }

                if (subsystem.TrySetTrackingOriginMode(m_TrackingOriginMode))
                {
                    trackingSettingsSet = subsystem.TryRecenter();
                }
            }

            if (trackingSettingsSet)
            {
                // Move camera to correct height
                MoveCameraHeight(desiredOffset);
            }

            return trackingSettingsSet;
        }
#else
        bool SetupCamera()
        {
            SetupCameraLegacy(m_TrackingSpace);
            return true;
        }
#endif // UNITY_2019_3_OR_NEWER

#pragma warning disable 0618 // Disable Obsolete warnings for TrackingSpaceType, explicitly to allow for using legacy data if available.
        void SetupCameraLegacy(TrackingSpaceType trackingSpaceType)
        {
            var desiredOffset = m_CameraYOffset;
            XRDevice.SetTrackingSpaceType(trackingSpaceType);
            switch (trackingSpaceType)
            {
                case TrackingSpaceType.Stationary:
                    InputTracking.Recenter();
                    break;
                case TrackingSpaceType.RoomScale:
                    desiredOffset = 0f;
                    break;
            }

            // Move camera to correct height
            MoveCameraHeight(desiredOffset);
        }
#pragma warning restore 0618

        /// <summary>
        /// Sets the height of the camera to the given <paramref name="y"/> value.
        /// </summary>
        /// <param name="y">The local y-position to set.</param>
        void MoveCameraHeight(float y)
        {
            if (m_CameraFloorOffsetObject != null)
            {
                var desiredPosition = m_CameraFloorOffsetObject.transform.localPosition;
                desiredPosition.y = y;
                m_CameraFloorOffsetObject.transform.localPosition = desiredPosition;
            }
        }

        /// <summary>
        /// Rotates the rig object around the camera object by the provided <paramref name="angleDegrees"/>.
        /// This rotation only occurs around the rig's Up vector
        /// </summary>
        /// <param name="angleDegrees">The amount of rotation in degrees.</param>
        /// <returns>Returns <see langword="true"/> if the rotation is performed. Returns <see langword="false"/> otherwise.</returns>
        public bool RotateAroundCameraUsingRigUp(float angleDegrees)
        {
            return RotateAroundCameraPosition(m_RigBaseGameObject.transform.up, angleDegrees);
        }

        /// <summary>
        /// Rotates the rig object around the camera object's position in world space using the provided <paramref name="vector"/>
        /// as the rotation axis. The rig object is rotated by the amount of degrees provided in <paramref name="angleDegrees"/>.
        /// </summary>
        /// <param name="vector">The axis of the rotation.</param>
        /// <param name="angleDegrees">The amount of rotation in degrees.</param>
        /// <returns>Returns <see langword="true"/> if the rotation is performed. Returns <see langword="false"/> otherwise.</returns>
        public bool RotateAroundCameraPosition(Vector3 vector, float angleDegrees)
        {
            if (m_CameraGameObject == null || m_RigBaseGameObject == null)
            {
                return false;
            }

            // Rotate around the camera position
            m_RigBaseGameObject.transform.RotateAround(m_CameraGameObject.transform.position, vector, angleDegrees);

            return true;
        }

        /// <summary>
        /// This function will rotate the rig object such that the rig's up vector will match the provided vector.
        /// </summary>
        /// <param name="destinationUp">the vector to which the rig object's up vector will be matched.</param>
        /// <returns>Returns <see langword="true"/> if the rotation is performed or the vectors have already been matched.
        /// Returns <see langword="false"/> otherwise.</returns>
        public bool MatchRigUp(Vector3 destinationUp)
        {
            if (m_RigBaseGameObject == null)
            {
                return false;
            }

            if (m_RigBaseGameObject.transform.up == destinationUp)
                return true;

            var rigUp = Quaternion.FromToRotation(m_RigBaseGameObject.transform.up, destinationUp);
            m_RigBaseGameObject.transform.rotation = rigUp * transform.rotation;
            
            return true;
        }

        /// <summary>
        /// This function will rotate the rig object around the camera object using the <paramref name="destinationUp"/> vector such that:
        /// <list type="bullet">
        /// <item>The camera will look at the area in the direction of the <paramref name="destinationForward"/></item>
        /// <item>The projection of camera's forward vector on the plane with the normal <paramref name="destinationUp"/> will be in the direction of <paramref name="destinationForward"/></item>
        /// <item>The up vector of the rig object will match the provided <paramref name="destinationUp"/> vector (note that the camera's Up vector can not be manipulated)</item>
        /// </list>
        /// </summary>
        /// <param name="destinationUp">The up vector that the rig's up vector will be matched to.</param>
        /// <param name="destinationForward">The forward vector that will be matched to the projection of the camera's forward vector on the plane with the normal <paramref name="destinationUp"/>.</param>
        /// <returns>Returns <see langword="true"/> if the rotation is performed. Returns <see langword="false"/> otherwise.</returns>
        public bool MatchRigUpCameraForward(Vector3 destinationUp, Vector3 destinationForward)
        {
            if (m_CameraGameObject != null && MatchRigUp(destinationUp))
            {
                // Project current camera's forward vector on the destination plane, whose normal vector is destinationUp.
                var projectedCamForward = Vector3.ProjectOnPlane(cameraGameObject.transform.forward, destinationUp).normalized;

                // The angle that we want the rig to rotate is the signed angle between projectedCamForward and destinationForward, after the up vectors are matched.
                var signedAngle = Vector3.SignedAngle(projectedCamForward, destinationForward, destinationUp);

                RotateAroundCameraPosition(destinationUp, signedAngle);

                return true;
            }

            return false;
        }

        /// <summary>
        /// This function will rotate the rig object around the camera object using the <paramref name="destinationUp"/> vector such that:
        /// <list type="bullet">
        /// <item>The forward vector of the rig object, which is the direction the player moves in Unity when walking forward in the physical world, will match the provided <paramref name="destinationUp"/> vector</item>
        /// <item>The up vector of the rig object will match the provided <paramref name="destinationUp"/> vector</item>
        /// </list>
        /// </summary>
        /// <param name="destinationUp">The up vector that the rig's up vector will be matched to.</param>
        /// <param name="destinationForward">The forward vector that will be matched to the forward vector of the rig object,
        /// which is the direction the player moves in Unity when walking forward in the physical world.</param>
        /// <returns>Returns <see langword="true"/> if the rotation is performed. Returns <see langword="false"/> otherwise.</returns>
        public bool MatchRigUpRigForward (Vector3 destinationUp, Vector3 destinationForward)
        {
            if (m_RigBaseGameObject != null && MatchRigUp(destinationUp))
            {
                // The angle that we want the rig to rotate is the signed angle between the rig's forward and destinationForward, after the up vectors are matched.
                var signedAngle = Vector3.SignedAngle(m_RigBaseGameObject.transform.forward, destinationForward, destinationUp);

                RotateAroundCameraPosition(destinationUp, signedAngle);

                return true;
            }

            return false;
        }

        /// <summary>
        /// This function moves the camera to the world location provided by desiredWorldLocation.
        /// It does this by moving the rig object so that the camera's world location matches the desiredWorldLocation
        /// </summary>
        /// <param name="desiredWorldLocation">the position in world space that the camera should be moved to</param>
        /// <returns>Returns <see langword="true"/> if the move is performed. Returns <see langword="false"/> otherwise.</returns>
        public bool MoveCameraToWorldLocation(Vector3 desiredWorldLocation)
        {
            if (m_CameraGameObject == null)
            {
                return false;
            }

            var rot = Matrix4x4.Rotate(cameraGameObject.transform.rotation);
            var delta = rot.MultiplyPoint3x4(rigInCameraSpacePos);
            m_RigBaseGameObject.transform.position = delta + desiredWorldLocation;

            return true;
        }
    }
}
