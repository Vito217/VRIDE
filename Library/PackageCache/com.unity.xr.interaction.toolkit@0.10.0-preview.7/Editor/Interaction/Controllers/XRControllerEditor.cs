using UnityEditor;

namespace UnityEngine.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRController)), CanEditMultipleObjects]
    public class XRControllerEditor : Editor
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
                  
            public static GUIContent controllerNodeLabel = EditorGUIUtility.TrTextContent("Controller Node", "The XRNode for this controller.");
            public static GUIContent selectUsageLabel = EditorGUIUtility.TrTextContent("Select Usage", "The input to use for detecting a select.");
            public static GUIContent activateUsageLabel = EditorGUIUtility.TrTextContent("Activate Usage", "The input to use for detecting activation.");
            public static GUIContent uiPressUsageLabel = EditorGUIUtility.TrTextContent("UI Press Usage", "The input to use for detecting a UI press.");
            public static GUIContent axisToPressThresholdLabel = EditorGUIUtility.TrTextContent("Axis To Press Threshold", "The amount an axis needs to be pressed to trigger an interaction event.");

            public static GUIContent rotateAnchorLeftLabel = EditorGUIUtility.TrTextContent("Rotate Object Left", "The input to use to rotate an anchor to the Left.");
            public static GUIContent rotateAnchorRightLabel = EditorGUIUtility.TrTextContent("Rotate Object Right", "The input to use to rotate an anchor to the Right.");
            public static GUIContent moveObjectInLabel = EditorGUIUtility.TrTextContent("Move Object In", "The input that will be used to translate the anchor away from the interactor.");
            public static GUIContent moveObjectOutLabel = EditorGUIUtility.TrTextContent("Move Object Out", "The input that will be used to translate the anchor towards the interactor.");

#if LIH_PRESENT	            
            public static GUIContent poseProviderLabel = EditorGUIUtility.TrTextContent("Pose Provider", "Pose provider used to provide tracking data separate from the XR Node."); 
            public static readonly string poseProviderWarning = "This XR Controller is using an external pose provider for tracking.  This takes priority over the Controller Node Setting."; 
#endif
        }

        SerializedProperty m_UpdateTrackingType;
        SerializedProperty m_EnableInputTracking;
        SerializedProperty m_EnableInputActions;
        SerializedProperty m_ModelPrefab;
        SerializedProperty m_ModelTransform;
        SerializedProperty m_AnimateModel;
        SerializedProperty m_ModelSelectTransition;
        SerializedProperty m_ModelDeSelectTransition;

        SerializedProperty m_ControllerNode;
        SerializedProperty m_SelectUsage;
        SerializedProperty m_ActivateUsage;
        SerializedProperty m_UiPressUsage;
        SerializedProperty m_AxisToPressThreshold;

        SerializedProperty m_RotateAnchorLeft;
        SerializedProperty m_RotateAnchorRight;
        SerializedProperty m_MoveObjectIn;
        SerializedProperty m_MoveObjectOut;

#if LIH_PRESENT
        SerializedProperty m_PoseProvider;
#endif

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
            m_ControllerNode = serializedObject.FindProperty("m_ControllerNode");
            m_SelectUsage = serializedObject.FindProperty("m_SelectUsage");
            m_ActivateUsage = serializedObject.FindProperty("m_ActivateUsage");
            m_UiPressUsage = serializedObject.FindProperty("m_UIPressUsage");
            m_AxisToPressThreshold = serializedObject.FindProperty("m_AxisToPressThreshold");
            m_RotateAnchorLeft = serializedObject.FindProperty("m_RotateAnchorLeft");
            m_RotateAnchorRight = serializedObject.FindProperty("m_RotateAnchorRight");
            m_MoveObjectIn = serializedObject.FindProperty("m_MoveObjectIn");
            m_MoveObjectOut = serializedObject.FindProperty("m_MoveObjectOut");

#if LIH_PRESENT
            m_PoseProvider = serializedObject.FindProperty("m_PoseProvider");
#endif
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRController)target), typeof(XRController), false);
            EditorGUI.EndDisabledGroup();

            EditorGUILayout.PropertyField(m_UpdateTrackingType, Styles.updateTrackingTypeLabel);
            EditorGUILayout.PropertyField(m_EnableInputTracking, Styles.enableInputTrackingLabel);
            EditorGUILayout.PropertyField(m_EnableInputActions, Styles.enableInputActionsLabel);

#if LIH_PRESENT	            
            EditorGUILayout.PropertyField(m_PoseProvider, Styles.poseProviderLabel);
            if (m_PoseProvider.objectReferenceValue != null)
            {
                EditorGUILayout.HelpBox(Styles.poseProviderWarning, MessageType.Info, true);
            }
#endif

            EditorGUILayout.PropertyField(m_ControllerNode, Styles.controllerNodeLabel);
            EditorGUILayout.PropertyField(m_SelectUsage, Styles.selectUsageLabel);
            EditorGUILayout.PropertyField(m_ActivateUsage, Styles.activateUsageLabel);
            EditorGUILayout.PropertyField(m_UiPressUsage, Styles.uiPressUsageLabel);
            EditorGUILayout.PropertyField(m_AxisToPressThreshold, Styles.axisToPressThresholdLabel);

            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(m_RotateAnchorLeft, Styles.rotateAnchorLeftLabel);
            EditorGUILayout.PropertyField(m_RotateAnchorRight, Styles.rotateAnchorRightLabel);
            EditorGUILayout.PropertyField(m_MoveObjectIn, Styles.moveObjectInLabel);
            EditorGUILayout.PropertyField(m_MoveObjectOut, Styles.moveObjectOutLabel);

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
