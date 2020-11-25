using UnityEditor;

namespace UnityEngine.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(ActionBasedController)), CanEditMultipleObjects]
    public class ActionBasedControllerEditor : Editor
    {
        static class Styles
        {
            public static GUIContent updateTrackingTypeLabel = EditorGUIUtility.TrTextContent("Update Tracking Type", "The time within the frame that the controller will sample input.");
            public static GUIContent enableInputTrackingLabel = EditorGUIUtility.TrTextContent("Enable Input Tracking", "Whether input tracking is enabled for this controller.");
            public static GUIContent enableInputActionsLabel = EditorGUIUtility.TrTextContent("Enable Input Actions", "Used to disable an input state changing in the interactor. Useful for swapping to a different interactor on the same object.");
            public static GUIContent modelPrefabLabel = EditorGUIUtility.TrTextContent("Model Prefab", "The model prefab to show for this controller.");
            public static GUIContent modelTransformLabel = EditorGUIUtility.TrTextContent("Model Transform", "The model transform that is used as the parent for the controller model.");
            public static GUIContent animateModelLabel = EditorGUIUtility.TrTextContent("Animate Model", "Whether this model animates in response to interaction events.");
            public static GUIContent modelSelectTransitionLabel = EditorGUIUtility.TrTextContent("Model Select Transition", "The animation transition to enable when selecting.");
            public static GUIContent modelDeSelectTransitionLabel = EditorGUIUtility.TrTextContent("Model Deselect Transition", "The animation transition to enable when de-selecting.");
                  
            public static GUIContent positionActionLabel = EditorGUIUtility.TrTextContent("Position Action", "The Input System action to use for Position Tracking for this GameObject. Must be a Vector3 Control.");
            public static GUIContent rotationActionLabel = EditorGUIUtility.TrTextContent("Rotation Action", "The Input System action to use for Rotation Tracking for this GameObject. Must be a Quaternion Control.");
            public static GUIContent selectActionLabel = EditorGUIUtility.TrTextContent("Select Action", "The Input System action to use for Selecting an Interactable. Must be a Button Control.");
            public static GUIContent activateUsageLabel = EditorGUIUtility.TrTextContent("Activate Action", "The Input System action to use for Activating a selected Interactable. Must be a Button Control.");
            public static GUIContent uiPressUsageLabel = EditorGUIUtility.TrTextContent("UI Press Action", "The Input System action to use for UI interaction. Must be a Button Control.");
            public static GUIContent hapticDeviceActionLabel = EditorGUIUtility.TrTextContent("Haptic Device Action", "The Input System action to use for identifying the device to send haptic impulses to. Can be any control type that will have an active control driving the action.");

            public static GUIContent rotateAnchorUsageLabel = EditorGUIUtility.TrTextContent("Rotate Anchor Action", "The Input System action to use for rotating the interactor's attach point. Must be a Vector2 Control. Will use the X-axis as the rotation input.");
            public static GUIContent translateAnchorUsageLabel = EditorGUIUtility.TrTextContent("Translate Anchor Action", "The Input System action to use for translating the interactor's attach point closer or further away from the interactor. Must be a Vector2 Control. Will use the Y-axis as the translation input.");

            public static GUIContent deadzoneUsageLabel = EditorGUIUtility.TrTextContent("Anchor Deadzone", "The vector component magnitude of the primary axis the input must be above for translating or rotating the interactor's attach point.");
            public static GUIContent offAxisDeadzoneUsageLabel = EditorGUIUtility.TrTextContent("Off-Axis Anchor Deadzone", "The vector component magnitude of the opposite axis the input must be below for translating or rotating the interactor's attach point.");
        }

        SerializedProperty m_UpdateTrackingType;
        SerializedProperty m_EnableInputTracking;
        SerializedProperty m_EnableInputActions;
        SerializedProperty m_ModelPrefab;
        SerializedProperty m_ModelTransform;
        SerializedProperty m_AnimateModel;
        SerializedProperty m_ModelSelectTransition;
        SerializedProperty m_ModelDeSelectTransition;

        SerializedProperty m_PositionAction;
        SerializedProperty m_RotationAction;
        SerializedProperty m_SelectAction;
        SerializedProperty m_ActivateAction;
        SerializedProperty m_UiPressAction;
        SerializedProperty m_HapticDeviceAction;

        SerializedProperty m_RotateAnchorAction;
        SerializedProperty m_TranslateAnchorAction;

        SerializedProperty m_Deadzone;
        SerializedProperty m_OffAxisDeadzone;

        protected void OnEnable()
        {
            m_UpdateTrackingType = serializedObject.FindProperty("m_UpdateTrackingType");
            m_EnableInputTracking = serializedObject.FindProperty("m_EnableInputTracking");
            m_EnableInputActions = serializedObject.FindProperty("m_EnableInputActions");
            m_ModelPrefab = serializedObject.FindProperty("m_ModelPrefab");
            m_ModelTransform = serializedObject.FindProperty("m_ModelTransform");
            m_AnimateModel = serializedObject.FindProperty("m_AnimateModel");
            m_ModelSelectTransition = serializedObject.FindProperty("m_ModelSelectTransition");
            m_ModelDeSelectTransition = serializedObject.FindProperty("m_ModelDeSelectTransition");

            m_PositionAction = serializedObject.FindProperty("m_PositionAction");
            m_RotationAction = serializedObject.FindProperty("m_RotationAction");
            m_SelectAction = serializedObject.FindProperty("m_SelectAction");
            m_ActivateAction = serializedObject.FindProperty("m_ActivateAction");
            m_UiPressAction = serializedObject.FindProperty("m_UIPressAction");
            m_HapticDeviceAction = serializedObject.FindProperty("m_HapticDeviceAction");

            m_RotateAnchorAction = serializedObject.FindProperty("m_RotateAnchorAction");
            m_TranslateAnchorAction = serializedObject.FindProperty("m_TranslateAnchorAction");

            m_Deadzone = serializedObject.FindProperty("m_AnchorControlDeadzone");
            m_OffAxisDeadzone = serializedObject.FindProperty("m_AnchorControlOffAxisDeadzone");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            GUI.enabled = false;
            EditorGUILayout.ObjectField("Script", MonoScript.FromMonoBehaviour((XRBaseController)target), typeof(ActionBasedController), false);
            GUI.enabled = true;

            EditorGUILayout.PropertyField(m_UpdateTrackingType, Styles.updateTrackingTypeLabel);
            EditorGUILayout.PropertyField(m_EnableInputTracking, Styles.enableInputTrackingLabel);
            EditorGUILayout.PropertyField(m_EnableInputActions, Styles.enableInputActionsLabel);

            EditorGUILayout.PropertyField(m_PositionAction, Styles.positionActionLabel);
            EditorGUILayout.PropertyField(m_RotationAction, Styles.rotationActionLabel);
            EditorGUILayout.PropertyField(m_SelectAction, Styles.selectActionLabel);
            EditorGUILayout.PropertyField(m_ActivateAction, Styles.activateUsageLabel);
            EditorGUILayout.PropertyField(m_UiPressAction, Styles.uiPressUsageLabel);
            EditorGUILayout.PropertyField(m_HapticDeviceAction, Styles.hapticDeviceActionLabel);

            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(m_RotateAnchorAction, Styles.rotateAnchorUsageLabel);
            EditorGUILayout.PropertyField(m_TranslateAnchorAction, Styles.translateAnchorUsageLabel);

            EditorGUILayout.PropertyField(m_Deadzone, Styles.deadzoneUsageLabel);
            EditorGUILayout.PropertyField(m_OffAxisDeadzone, Styles.offAxisDeadzoneUsageLabel);

            EditorGUILayout.PropertyField(m_ModelPrefab, Styles.modelPrefabLabel);
            EditorGUILayout.PropertyField(m_ModelTransform, Styles.modelTransformLabel);
            EditorGUILayout.PropertyField(m_AnimateModel, Styles.animateModelLabel);

            if (m_AnimateModel.boolValue)
            {
                EditorGUI.indentLevel++;
                EditorGUILayout.PropertyField(m_ModelSelectTransition, Styles.modelSelectTransitionLabel);
                EditorGUILayout.PropertyField(m_ModelDeSelectTransition, Styles.modelDeSelectTransitionLabel);
                EditorGUI.indentLevel--;
            }

            serializedObject.ApplyModifiedProperties();
        }
    }
}
