using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

namespace UnityEditor.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRDirectInteractor)), CanEditMultipleObjects]
    public class XRDirectInteractorEditor : Editor
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

        SerializedProperty m_OnHoverEntered;
        SerializedProperty m_OnHoverExited;
        SerializedProperty m_OnSelectEntered;
        SerializedProperty m_OnSelectExited;

        static class Tooltips
        {
            public static readonly GUIContent interactionManager = new GUIContent("Interaction Manager", "Manager to handle all interaction management (will find one if empty).");
            public static readonly GUIContent interactionLayerMask = new GUIContent("Interaction Layer Mask", "Only interactables with this Layer Mask will respond to this interactor.");
            public static readonly GUIContent attachTransform = new GUIContent("Attach Transform", "Attach Transform to use for this Interactor.  Will create empty GameObject if none set.");
            public static readonly GUIContent startingSelectedInteractable = new GUIContent("Starting Selected Interactable", "Interactable that will be selected upon start.");
            public static readonly GUIContent selectActionTrigger = new GUIContent("Select Action Trigger", "Choose whether the select action is triggered by current state or state transitions.");
            public static readonly GUIContent hideControllerOnSelect = new GUIContent("Hide Controller On Select", "Hide controller on select.");

            public static readonly GUIContent playAudioClipOnSelectEntered = new GUIContent("On Select Entered", "Play an audio clip when the Select state is entered.");
            public static readonly GUIContent audioClipForOnSelectEntered = new GUIContent("AudioClip To Play", "The audio clip to play when the Select state is entered.");
            public static readonly GUIContent playAudioClipOnSelectExited = new GUIContent("On Select Exited", "Play an audio clip when the Select state is exited.");
            public static readonly GUIContent audioClipForOnSelectExited = new GUIContent("AudioClip To Play", "The audio clip to play when the Select state is exited.");
            public static readonly GUIContent playAudioClipOnHoverEntered = new GUIContent("On Hover Entered", "Play an audio clip when the Hover state is entered.");
            public static readonly GUIContent audioClipForOnHoverEntered = new GUIContent("AudioClip To Play", "The audio clip to play when the Hover state is entered.");
            public static readonly GUIContent playAudioClipOnHoverExited = new GUIContent("On Hover Exited", "Play an audio clip when the Hover state is exited.");
            public static readonly GUIContent audioClipForOnHoverExited = new GUIContent("AudioClip To Play", "The audio clip to play when the Hover state is exited.");

            public static readonly GUIContent playHapticsOnSelectEntered = new GUIContent("On Select Entered", "Play haptics when the select state is entered.");
            public static readonly GUIContent hapticSelectEnterIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Select state is entered.");
            public static readonly GUIContent hapticSelectEnterDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Select state is entered.");
            public static readonly GUIContent playHapticsOnHoverEntered = new GUIContent("On Hover Entered", "Play haptics when the hover state is entered.");
            public static readonly GUIContent hapticHoverEnterIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Hover state is entered.");
            public static readonly GUIContent hapticHoverEnterDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Hover state is entered.");
            public static readonly GUIContent playHapticsOnSelectExited = new GUIContent("On Select Exited", "Play haptics when the select state is exited.");
            public static readonly GUIContent hapticSelectExitIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Select state is exited.");
            public static readonly GUIContent hapticSelectExitDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Select state is exited.");
            public static readonly GUIContent playHapticsOnHoverExited = new GUIContent("On Hover Exited", "Play haptics when the Hover state is exited.");
            public static readonly GUIContent hapticHoverExitIntensity = new GUIContent("Haptic Intensity", "Haptics intensity to play when the Hover state is exited.");
            public static readonly GUIContent hapticHoverExitDuration = new GUIContent("Duration", "Haptics duration (in seconds) to play when the Hover state is exited.");

            public static readonly string startingInteractableWarning = "A Starting Selected Interactable will be instantly deselected unless the Interactor's Toggle Select Mode is set to 'Toggle' or 'Sticky'.";
            public static readonly string missingRequiredController = "XR Direct Interactor requires the GameObject to have an XR Controller component. Add one to ensure this component can respond to user input.";

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

            m_OnSelectEntered = serializedObject.FindProperty("m_OnSelectEntered");
            m_OnSelectExited = serializedObject.FindProperty("m_OnSelectExited");
            m_OnHoverEntered = serializedObject.FindProperty("m_OnHoverEntered");
            m_OnHoverExited = serializedObject.FindProperty("m_OnHoverExited");

        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRDirectInteractor)target), typeof(XRDirectInteractor), false);
            EditorGUI.EndDisabledGroup();

            foreach (var targetObject in serializedObject.targetObjects)
            {
                var interactor = (XRDirectInteractor)targetObject;
                if (interactor.GetComponent<XRBaseController>() == null)
                {
                    EditorGUILayout.HelpBox(Tooltips.missingRequiredController, MessageType.Warning, true);
                    break;
                }
            }

            EditorGUILayout.PropertyField(m_InteractionManager, Tooltips.interactionManager);
            EditorGUILayout.PropertyField(m_InteractionLayerMask, Tooltips.interactionLayerMask);
            EditorGUILayout.PropertyField(m_AttachTransform, Tooltips.attachTransform);

            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(m_SelectActionTrigger, Tooltips.selectActionTrigger);
            EditorGUILayout.PropertyField(m_HideControllerOnSelect, Tooltips.hideControllerOnSelect);
            EditorGUILayout.PropertyField(m_StartingSelectedInteractable, Tooltips.startingSelectedInteractable);
            if (m_StartingSelectedInteractable.objectReferenceValue != null
                && (m_SelectActionTrigger.enumValueIndex == (int)XRBaseControllerInteractor.InputTriggerType.Sticky
                 || m_SelectActionTrigger.enumValueIndex == (int)XRBaseControllerInteractor.InputTriggerType.Toggle))
            {
                EditorGUILayout.HelpBox(Tooltips.startingInteractableWarning, MessageType.Warning, true);
            }

            EditorGUILayout.Space();

            m_PlayAudioClipOnSelectEntered.isExpanded = EditorGUILayout.Foldout(m_PlayAudioClipOnSelectEntered.isExpanded, EditorGUIUtility.TrTempContent("Audio Events"), true);
            if (m_PlayAudioClipOnSelectEntered.isExpanded)
            {
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
            }

            EditorGUILayout.Space();

            m_PlayHapticsOnSelectEntered.isExpanded = EditorGUILayout.Foldout(m_PlayHapticsOnSelectEntered.isExpanded, EditorGUIUtility.TrTempContent("Haptic Events"), true);
            if (m_PlayHapticsOnSelectEntered.isExpanded)
            {
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
