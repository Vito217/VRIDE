using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

namespace UnityEditor.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRRayInteractor)), CanEditMultipleObjects]
    public class XRRayInteractorEditor : Editor
    {
        SerializedProperty m_InteractionManager;
        SerializedProperty m_InteractionLayerMask;
        SerializedProperty m_AttachTransform;
        SerializedProperty m_StartingSelectedInteractable;
        SerializedProperty m_SelectActionTrigger;
        SerializedProperty m_HideControllerOnSelect;

        SerializedProperty m_PlayAudioClipOnSelectEntered;
        SerializedProperty m_AudioClipForOnSelectEntered;
        SerializedProperty m_PlayAudioClipOnSelectExited;
        SerializedProperty m_AudioClipForOnSelectExited;
        SerializedProperty m_PlayAudioClipOnHoverEntered;
        SerializedProperty m_AudioClipForOnHoverEntered;
        SerializedProperty m_PlayAudioClipOnHoverExited;
        SerializedProperty m_AudioClipForOnHoverExited;

        SerializedProperty m_PlayHapticsOnSelectEntered;
        SerializedProperty m_HapticSelectEnterIntensity;
        SerializedProperty m_HapticSelectEnterDuration;
        SerializedProperty m_PlayHapticsOnHoverEntered;
        SerializedProperty m_HapticHoverEnterIntensity;
        SerializedProperty m_HapticHoverEnterDuration;
        SerializedProperty m_PlayHapticsOnSelectExited;
        SerializedProperty m_HapticSelectExitIntensity;
        SerializedProperty m_HapticSelectExitDuration;
        SerializedProperty m_PlayHapticsOnHoverExited;
        SerializedProperty m_HapticHoverExitIntensity;
        SerializedProperty m_HapticHoverExitDuration;

        SerializedProperty m_MaxRaycastDistance;
        SerializedProperty m_HitDetectionType;
        SerializedProperty m_SphereCastRadius;
        SerializedProperty m_RaycastMask;
        SerializedProperty m_RaycastTriggerInteraction;
        SerializedProperty m_HoverToSelect;
        SerializedProperty m_HoverTimeToSelect;
        SerializedProperty m_EnableUIInteraction;

        SerializedProperty m_LineType;
        SerializedProperty m_EndPointDistance;
        SerializedProperty m_EndPointHeight;
        SerializedProperty m_ControlPointDistance;
        SerializedProperty m_ControlPointHeight;
        SerializedProperty m_SampleFrequency;

        SerializedProperty m_Velocity;
        SerializedProperty m_Acceleration;
        SerializedProperty m_AdditionalFlightTime;
        SerializedProperty m_ReferenceFrame;

        SerializedProperty m_OnHoverEntered;
        SerializedProperty m_OnHoverExited;
        SerializedProperty m_OnSelectEntered;
        SerializedProperty m_OnSelectExited;

        SerializedProperty m_KeepSelectedTargetValid;
        SerializedProperty m_AllowAnchorControl;
        SerializedProperty m_UseForceGrab;

        SerializedProperty m_AnchorRotateSpeed;
        SerializedProperty m_AnchorTranslateSpeed;

        static class Tooltips
        {
            public static readonly GUIContent interactionManager = new GUIContent("Interaction Manager", "Manager to handle all interaction management (will find one if empty).");
            public static readonly GUIContent interactionLayerMask = new GUIContent("Interaction Layer Mask", "Only interactables with this Layer Mask will respond to this interactor.");
            public static readonly GUIContent attachTransform = new GUIContent("Attach Transform", "Attach Transform to use for this Interactor.  Will create empty GameObject if none set.");
            public static readonly GUIContent startingSelectedInteractable = new GUIContent("Starting Selected Interactable", "Interactable that will be selected upon start.");
            public static readonly GUIContent selectActionTrigger = new GUIContent("Select Action Trigger", "Choose how the select action is triggered by current state, state transitions, toggle when the select button is pressed, or [Sticky] toggle on when the select button is pressed and off the second time the select button is depressed.");
            public static readonly GUIContent hideControllerOnSelect = new GUIContent("Hide Controller On Select", "Hide controller on select.");

            public static readonly GUIContent playAudioClipOnSelectEntered = new GUIContent("On Select Entered", "Play an audio clip when the Select state is entered.");
            public static readonly GUIContent audioClipForOnSelectEntered = new GUIContent("AudioClip To Play", "The audio clip to play when the Select state is entered.");
            public static readonly GUIContent playAudioClipOnSelectExited = new GUIContent("On Select Exited", "Play an audio clip when the Select state is exited.");
            public static readonly GUIContent audioClipForOnSelectExited = new GUIContent("AudioClip To Play", "The audio clip to play when the Select state is exited.");
            public static readonly GUIContent playAudioClipOnHoverEntered = new GUIContent("On Hover Entered", "Play an audio clip when the Hover state is entered.");
            public static readonly GUIContent audioClipForOnHoverEntered = new GUIContent("AudioClip To Play", "The audio clip to play when the Hover state is entered.");
            public static readonly GUIContent playAudioClipOnHoverExited = new GUIContent("On Hover Exited", "Play an audio clip when the Hover state is exited.");
            public static readonly GUIContent audioClipForOnHoverExited = new GUIContent("AudioClip To Play", "The audio clip to play when the Hover state is exited.");

            public static readonly GUIContent playHapticsOnSelectEntered = new GUIContent("On Select Entered", "Play haptics when the Select state is entered.");
            public static readonly GUIContent hapticSelectEnterIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Select state is entered.");
            public static readonly GUIContent hapticSelectEnterDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Select state is entered.");
            public static readonly GUIContent playHapticsOnHoverEntered = new GUIContent("On Hover Entered", "Play haptics when the Hover State is entered.");
            public static readonly GUIContent hapticHoverEnterIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Hover state is entered.");
            public static readonly GUIContent hapticHoverEnterDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Hover state is entered.");
            public static readonly GUIContent playHapticsOnSelectExited = new GUIContent("On Select Exited", "Play haptics when the Select state is exited.");
            public static readonly GUIContent hapticSelectExitIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Select state is exited.");
            public static readonly GUIContent hapticSelectExitDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Select state is exited.");
            public static readonly GUIContent playHapticsOnHoverExited = new GUIContent("On Hover Exited", "Play haptics when the Hover state is exited.");
            public static readonly GUIContent hapticHoverExitIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Hover state is exited.");
            public static readonly GUIContent hapticHoverExitDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Hover state is exited.");

            public static readonly GUIContent maxRaycastDistance = new GUIContent("Max Raycast Distance", "Max distance of ray cast. Increase this value will let you reach further.");
            public static readonly GUIContent sphereCastRadius = new GUIContent("Sphere Cast Radius", "Radius of this Interactor's ray, used for sphere casting.");
            public static readonly GUIContent raycastMask = new GUIContent("Raycast Mask", "Layer mask used for limiting raycast targets.");
            public static readonly GUIContent raycastTriggerInteraction = new GUIContent("Raycast Trigger Interaction", "Type of interaction with trigger colliders via raycast.");
            public static readonly GUIContent hoverToSelect = new GUIContent("Hover To Select", "If true, this interactor will simulate a Select event if hovered over an Interactable for some amount of time. Selection will be exited when the Interactor is no longer hovering over the Interactable.");
            public static readonly GUIContent hoverTimeToSelect = new GUIContent("Hover Time To Select", "Number of seconds for which this interactor must hover over an object to select it.");
            public static readonly GUIContent enableUIInteraction = new GUIContent("Enable Interaction with UI GameObjects", "If checked, this interactor will be able to affect UI.");
            public static readonly GUIContent lineType = new GUIContent("Line Type", "Line type of the ray cast.");
            public static readonly GUIContent endPointDistance = new GUIContent("End Point Distance", "Increase this value distance will make the end of curve further from the start point.");
            public static readonly GUIContent controlPointDistance = new GUIContent("Control Point Distance", "Increase this value will make the peak of the curve further from the start point.");
            public static readonly GUIContent endPointHeight = new GUIContent("End Point Height", "Decrease this value will make the end of the curve drop lower relative to the start point.");
            public static readonly GUIContent controlPointHeight = new GUIContent("Control Point Height", "Increase this value will make the peak of the curve higher relative to the start point.");
            public static readonly GUIContent sampleFrequency = new GUIContent("Sample Frequency", "Gets or sets the number of sample points of the curve, should be at least 3, the higher the better quality.");
            public static readonly GUIContent velocity = new GUIContent("Velocity", "Initial velocity of the projectile. Increase this value will make the curve reach further.");
            public static readonly GUIContent acceleration = new GUIContent("Acceleration", "Gravity of the projectile in the reference frame.");
            public static readonly GUIContent additionalFlightTime = new GUIContent("Additional Flight Time", "Additional flight time after the projectile lands at the same height of the start point in the tracking space. Increase this value will make the end point drop lower in height.");
            public static readonly GUIContent referenceFrame = new GUIContent("Reference Frame", "The reference frame of the projectile. If not set it will try to find the XRRig GameObject, and if that does not exist it will use its own Transform.");
            public static readonly GUIContent hitDetectionType = new GUIContent("Hit Detection Type", "The type of hit detection used to hit interactable objects.");

            public static readonly GUIContent keepSelectedTargetValid = new GUIContent("Keep Selected Target Valid", "Keep selecting the target when not pointing to it after initially selecting it. It is recommended to set this value to true for grabbing objects, false for teleportation interactables.");
            public static readonly GUIContent allowAnchorControl = new GUIContent("Anchor Control", "Allows the user to move the attach anchor point using the joystick.");
            public static readonly GUIContent forceGrab = new GUIContent("Force Grab", "Force grab moves the object to your hand rather than interacting with it at a distance.");
            public static readonly GUIContent anchorRotateSpeed = new GUIContent("Rotate Speed", "Speed that the anchor is rotated.");
            public static readonly GUIContent anchorTranslateSpeed = new GUIContent("Translate Speed", "Speed that the anchor is translated.");

            public static readonly string startingInteractableWarning = "A Starting Selected Interactable will be instantly deselected unless the Interactor's Toggle Select Mode is set to 'Toggle' or 'Sticky'.";
            public static readonly string missingRequiredController = "XR Ray Interactor requires the GameObject to have an XR Controller component. Add one to ensure this component can respond to user input.";
        }

        protected void OnEnable()
        {
            m_InteractionManager = serializedObject.FindProperty("m_InteractionManager");
            m_InteractionLayerMask = serializedObject.FindProperty("m_InteractionLayerMask");
            m_AttachTransform = serializedObject.FindProperty("m_AttachTransform");
            m_StartingSelectedInteractable = serializedObject.FindProperty("m_StartingSelectedInteractable");
            m_SelectActionTrigger = serializedObject.FindProperty("m_SelectActionTrigger");
            m_HideControllerOnSelect = serializedObject.FindProperty("m_HideControllerOnSelect");
            m_PlayAudioClipOnSelectEntered = serializedObject.FindProperty("m_PlayAudioClipOnSelectEntered");
            m_AudioClipForOnSelectEntered = serializedObject.FindProperty("m_AudioClipForOnSelectEntered");
            m_PlayAudioClipOnSelectExited = serializedObject.FindProperty("m_PlayAudioClipOnSelectExited");
            m_AudioClipForOnSelectExited = serializedObject.FindProperty("m_AudioClipForOnSelectExited");
            m_PlayAudioClipOnHoverEntered = serializedObject.FindProperty("m_PlayAudioClipOnHoverEntered");
            m_AudioClipForOnHoverEntered = serializedObject.FindProperty("m_AudioClipForOnHoverEntered");
            m_PlayAudioClipOnHoverExited = serializedObject.FindProperty("m_PlayAudioClipOnHoverExited");
            m_AudioClipForOnHoverExited = serializedObject.FindProperty("m_AudioClipForOnHoverExited");
            m_PlayHapticsOnSelectEntered = serializedObject.FindProperty("m_PlayHapticsOnSelectEntered");
            m_HapticSelectEnterIntensity = serializedObject.FindProperty("m_HapticSelectEnterIntensity");
            m_HapticSelectEnterDuration = serializedObject.FindProperty("m_HapticSelectEnterDuration");
            m_PlayHapticsOnHoverEntered = serializedObject.FindProperty("m_PlayHapticsOnHoverEntered");
            m_HapticHoverEnterIntensity = serializedObject.FindProperty("m_HapticHoverEnterIntensity");
            m_HapticHoverEnterDuration = serializedObject.FindProperty("m_HapticHoverEnterDuration");
            m_PlayHapticsOnSelectExited = serializedObject.FindProperty("m_PlayHapticsOnSelectExited");
            m_HapticSelectExitIntensity = serializedObject.FindProperty("m_HapticSelectExitIntensity");
            m_HapticSelectExitDuration = serializedObject.FindProperty("m_HapticSelectExitDuration");
            m_PlayHapticsOnHoverExited = serializedObject.FindProperty("m_PlayHapticsOnHoverExited");
            m_HapticHoverExitIntensity = serializedObject.FindProperty("m_HapticHoverExitIntensity");
            m_HapticHoverExitDuration = serializedObject.FindProperty("m_HapticHoverExitDuration");
            m_MaxRaycastDistance = serializedObject.FindProperty("m_MaxRaycastDistance");
            m_SphereCastRadius = serializedObject.FindProperty("m_SphereCastRadius");
            m_HitDetectionType = serializedObject.FindProperty("m_HitDetectionType");
            m_RaycastMask = serializedObject.FindProperty("m_RaycastMask");
            m_RaycastTriggerInteraction = serializedObject.FindProperty("m_RaycastTriggerInteraction");
            m_HoverToSelect = serializedObject.FindProperty("m_HoverToSelect");
            m_HoverTimeToSelect = serializedObject.FindProperty("m_HoverTimeToSelect");
            m_EnableUIInteraction = serializedObject.FindProperty("m_EnableUIInteraction");

            m_LineType = serializedObject.FindProperty("m_LineType");
            m_EndPointDistance = serializedObject.FindProperty("m_EndPointDistance");
            m_EndPointHeight = serializedObject.FindProperty("m_EndPointHeight");
            m_ControlPointDistance = serializedObject.FindProperty("m_ControlPointDistance");
            m_ControlPointHeight = serializedObject.FindProperty("m_ControlPointHeight");
            m_SampleFrequency = serializedObject.FindProperty("m_SampleFrequency");

            m_ReferenceFrame = serializedObject.FindProperty("m_ReferenceFrame");
            m_Velocity = serializedObject.FindProperty("m_Velocity");
            m_Acceleration = serializedObject.FindProperty("m_Acceleration");
            m_AdditionalFlightTime = serializedObject.FindProperty("m_AdditionalFlightTime");

            m_OnHoverEntered = serializedObject.FindProperty("m_OnHoverEntered");
            m_OnHoverExited = serializedObject.FindProperty("m_OnHoverExited");
            m_OnSelectEntered = serializedObject.FindProperty("m_OnSelectEntered");
            m_OnSelectExited = serializedObject.FindProperty("m_OnSelectExited");

            m_KeepSelectedTargetValid = serializedObject.FindProperty("m_KeepSelectedTargetValid");
            m_AllowAnchorControl = serializedObject.FindProperty("m_AllowAnchorControl");
            m_UseForceGrab = serializedObject.FindProperty("m_UseForceGrab");
            m_AnchorRotateSpeed = serializedObject.FindProperty("m_RotateSpeed");
            m_AnchorTranslateSpeed = serializedObject.FindProperty("m_TranslateSpeed");

            // Set default expanded for some foldouts
            const string initializedKey = "XRI." + nameof(XRRayInteractorEditor) + ".Initialized";
            if (!SessionState.GetBool(initializedKey, false))
            {
                SessionState.SetBool(initializedKey, true);
                m_LineType.isExpanded = true;
                m_SelectActionTrigger.isExpanded = true;
            }
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRRayInteractor)target), typeof(XRRayInteractor), false);
            EditorGUI.EndDisabledGroup();

            foreach (var targetObject in serializedObject.targetObjects)
            {
                var interactor = (XRRayInteractor)targetObject;
                if (interactor.GetComponent<XRBaseController>() == null)
                {
                    EditorGUILayout.HelpBox(Tooltips.missingRequiredController, MessageType.Warning, true);
                    break;
                }
            }

            // Interaction Management
            EditorGUILayout.PropertyField(m_InteractionManager, Tooltips.interactionManager);
            EditorGUILayout.PropertyField(m_InteractionLayerMask, Tooltips.interactionLayerMask);

            EditorGUILayout.Space();
            // End of Interaction Management

            // Interactable
            EditorGUILayout.PropertyField(m_EnableUIInteraction, Tooltips.enableUIInteraction);
            EditorGUILayout.PropertyField(m_UseForceGrab, Tooltips.forceGrab);
            EditorGUILayout.PropertyField(m_AllowAnchorControl, Tooltips.allowAnchorControl);
            EditorGUI.indentLevel++;
            if (m_AllowAnchorControl.boolValue)
            {
                EditorGUILayout.PropertyField(m_AnchorRotateSpeed, Tooltips.anchorRotateSpeed);
                EditorGUILayout.PropertyField(m_AnchorTranslateSpeed, Tooltips.anchorTranslateSpeed);
            }
            EditorGUI.indentLevel--;

            EditorGUILayout.PropertyField(m_AttachTransform, Tooltips.attachTransform);

            EditorGUILayout.Space();
            // End of Interactable

            // Raycast Configuration
            m_LineType.isExpanded = EditorGUILayout.Foldout(m_LineType.isExpanded, EditorGUIUtility.TrTempContent("Raycast Configuration"), true);
            if (m_LineType.isExpanded)
            {
                EditorGUI.indentLevel++;

                // Line type
                EditorGUILayout.PropertyField(m_LineType, Tooltips.lineType);

                EditorGUI.indentLevel++;
                switch (m_LineType.enumValueIndex)
                {
                    case (int)XRRayInteractor.LineType.StraightLine:
                        EditorGUILayout.PropertyField(m_MaxRaycastDistance, Tooltips.maxRaycastDistance);
                        break;
                    case (int)XRRayInteractor.LineType.ProjectileCurve:
                        EditorGUILayout.PropertyField(m_ReferenceFrame, Tooltips.referenceFrame);
                        EditorGUILayout.PropertyField(m_Velocity, Tooltips.velocity);
                        EditorGUILayout.PropertyField(m_Acceleration, Tooltips.acceleration);
                        EditorGUILayout.PropertyField(m_AdditionalFlightTime, Tooltips.additionalFlightTime);
                        EditorGUILayout.PropertyField(m_SampleFrequency, Tooltips.sampleFrequency);
                        break;
                    case (int)XRRayInteractor.LineType.BezierCurve:
                        EditorGUILayout.PropertyField(m_EndPointDistance, Tooltips.endPointDistance);
                        EditorGUILayout.PropertyField(m_EndPointHeight, Tooltips.endPointHeight);
                        EditorGUILayout.PropertyField(m_ControlPointDistance, Tooltips.controlPointDistance);
                        EditorGUILayout.PropertyField(m_ControlPointHeight, Tooltips.controlPointHeight);
                        EditorGUILayout.PropertyField(m_SampleFrequency, Tooltips.sampleFrequency);
                        break;
                }
                EditorGUI.indentLevel--;

                EditorGUILayout.Space();
                // End of Line type

                EditorGUILayout.PropertyField(m_RaycastMask, Tooltips.raycastMask);
                EditorGUILayout.PropertyField(m_RaycastTriggerInteraction, Tooltips.raycastTriggerInteraction);
                EditorGUILayout.PropertyField(m_HitDetectionType, Tooltips.hitDetectionType);
                if (m_HitDetectionType.enumValueIndex == (int)XRRayInteractor.HitDetectionType.SphereCast)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_SphereCastRadius, Tooltips.sphereCastRadius);
                    EditorGUI.indentLevel--;
                }

                EditorGUI.indentLevel--;
            }

            EditorGUILayout.Space();
            // End of Raycast Configuration

            // Selection Configuration
            m_SelectActionTrigger.isExpanded = EditorGUILayout.Foldout(m_SelectActionTrigger.isExpanded, EditorGUIUtility.TrTempContent("Selection Configuration"), true);
            if (m_SelectActionTrigger.isExpanded)
            {
                EditorGUI.indentLevel++;

                EditorGUILayout.PropertyField(m_SelectActionTrigger, Tooltips.selectActionTrigger);
                if (m_StartingSelectedInteractable.objectReferenceValue != null
                    && (m_SelectActionTrigger.enumValueIndex == (int)XRBaseControllerInteractor.InputTriggerType.Sticky
                     || m_SelectActionTrigger.enumValueIndex == (int)XRBaseControllerInteractor.InputTriggerType.Toggle))
                {
                    EditorGUILayout.HelpBox(Tooltips.startingInteractableWarning, MessageType.Warning, true);
                }

                EditorGUILayout.PropertyField(m_KeepSelectedTargetValid, Tooltips.keepSelectedTargetValid);
                EditorGUILayout.PropertyField(m_HideControllerOnSelect, Tooltips.hideControllerOnSelect);
                EditorGUILayout.PropertyField(m_HoverToSelect, Tooltips.hoverToSelect);
                if (m_HoverToSelect.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_HoverTimeToSelect, Tooltips.hoverTimeToSelect);
                    EditorGUI.indentLevel--;
                }
                EditorGUILayout.PropertyField(m_StartingSelectedInteractable, Tooltips.startingSelectedInteractable);

                EditorGUI.indentLevel--;

            }

            EditorGUILayout.Space();
            // End of Selection Configuration

            m_PlayAudioClipOnSelectEntered.isExpanded = EditorGUILayout.Foldout(m_PlayAudioClipOnSelectEntered.isExpanded, EditorGUIUtility.TrTempContent("Audio Events"), true);
            if (m_PlayAudioClipOnSelectEntered.isExpanded)
            {
                EditorGUI.indentLevel++;

                EditorGUILayout.PropertyField(m_PlayAudioClipOnSelectEntered, Tooltips.playAudioClipOnSelectEntered);
                if (m_PlayAudioClipOnSelectEntered.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_AudioClipForOnSelectEntered, Tooltips.audioClipForOnSelectEntered);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayAudioClipOnSelectExited, Tooltips.playAudioClipOnSelectExited);
                if (m_PlayAudioClipOnSelectExited.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_AudioClipForOnSelectExited, Tooltips.audioClipForOnSelectExited);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayAudioClipOnHoverEntered, Tooltips.playAudioClipOnHoverEntered);
                if (m_PlayAudioClipOnHoverEntered.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_AudioClipForOnHoverEntered, Tooltips.audioClipForOnHoverEntered);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayAudioClipOnHoverExited, Tooltips.playAudioClipOnHoverExited);
                if (m_PlayAudioClipOnHoverExited.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_AudioClipForOnHoverExited, Tooltips.audioClipForOnHoverExited);
                    EditorGUI.indentLevel--;
                }

                EditorGUI.indentLevel--;
            }

            EditorGUILayout.Space();

            m_PlayHapticsOnSelectEntered.isExpanded = EditorGUILayout.Foldout(m_PlayHapticsOnSelectEntered.isExpanded, EditorGUIUtility.TrTempContent("Haptic Events"), true);
            if (m_PlayHapticsOnSelectEntered.isExpanded)
            {
                EditorGUI.indentLevel++;

                EditorGUILayout.PropertyField(m_PlayHapticsOnSelectEntered, Tooltips.playHapticsOnSelectEntered);
                if (m_PlayHapticsOnSelectEntered.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_HapticSelectEnterIntensity, Tooltips.hapticSelectEnterIntensity);
                    EditorGUILayout.PropertyField(m_HapticSelectEnterDuration, Tooltips.hapticSelectEnterDuration);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayHapticsOnSelectExited, Tooltips.playHapticsOnSelectExited);
                if (m_PlayHapticsOnSelectExited.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_HapticSelectExitIntensity, Tooltips.hapticSelectExitIntensity);
                    EditorGUILayout.PropertyField(m_HapticSelectExitDuration, Tooltips.hapticSelectExitDuration);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayHapticsOnHoverEntered, Tooltips.playHapticsOnHoverEntered);
                if (m_PlayHapticsOnHoverEntered.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_HapticHoverEnterIntensity, Tooltips.hapticHoverEnterIntensity);
                    EditorGUILayout.PropertyField(m_HapticHoverEnterDuration, Tooltips.hapticHoverEnterDuration);
                    EditorGUI.indentLevel--;
                }

                EditorGUILayout.PropertyField(m_PlayHapticsOnHoverExited, Tooltips.playHapticsOnHoverExited);
                if (m_PlayHapticsOnHoverExited.boolValue)
                {
                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(m_HapticHoverExitIntensity, Tooltips.hapticHoverExitIntensity);
                    EditorGUILayout.PropertyField(m_HapticHoverExitDuration, Tooltips.hapticHoverExitDuration);
                    EditorGUI.indentLevel--;
                }

                EditorGUI.indentLevel--;
            }

            EditorGUILayout.Space();

            m_OnHoverEntered.isExpanded = EditorGUILayout.Foldout(m_OnHoverEntered.isExpanded, EditorGUIUtility.TrTempContent("Interactor Events"), true);
            if (m_OnHoverEntered.isExpanded)
            {
                EditorGUILayout.PropertyField(m_OnHoverEntered);
                EditorGUILayout.PropertyField(m_OnHoverExited);
                EditorGUILayout.PropertyField(m_OnSelectEntered);
                EditorGUILayout.PropertyField(m_OnSelectExited);
            }

            serializedObject.ApplyModifiedProperties();
        }
    }
}
