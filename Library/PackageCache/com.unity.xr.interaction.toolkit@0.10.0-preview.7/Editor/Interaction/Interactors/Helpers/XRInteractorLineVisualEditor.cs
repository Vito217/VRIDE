using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

namespace UnityEditor.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRInteractorLineVisual)), CanEditMultipleObjects]
    public class XRInteractorLineVisualEditor : Editor
    {
        SerializedProperty m_LineWidth;
        SerializedProperty m_WidthCurve;
        SerializedProperty m_ValidColorGradient;
        SerializedProperty m_InvalidColorGradient;
        SerializedProperty m_SmoothMovement;
        SerializedProperty m_FollowTightness;
        SerializedProperty m_SnapThresholdDistance;        
        SerializedProperty m_Reticle;
        SerializedProperty m_OverrideInteractorLineLength;
        SerializedProperty m_LineLength;
        SerializedProperty m_StopLineAtFirstRaycastHit;

        static class Tooltips
        {
            public static readonly GUIContent lineWidth = new GUIContent("Line Width", "Controls the width of the line.");
            public static readonly GUIContent widthCurve = new GUIContent("Width Curve", "Controls the relative width of the line from start to end.");
            public static readonly GUIContent validColorGradient = new GUIContent("Valid Color Gradient", "Controls the color of the line as a gradient from start to end to indicate a valid state.");
            public static readonly GUIContent invalidColorGradient = new GUIContent("Invalid Color Gradient", "Controls the color of the line as a gradient from start to end to indicate an invalid state.");
            public static readonly GUIContent smoothMovement = new GUIContent("Smooth Movement", "Controls whether the rendered segments will be delayed from and smoothly follow the target segments.");
            public static readonly GUIContent followTightness = new GUIContent("Follow Tightness", "Controls the speed that the rendered segments will follow the target segments when Smooth Movement is enabled.");
            public static readonly GUIContent snapThresholdDistance = new GUIContent("Snap Threshold Distance", "Controls the threshold distance between line points at two consecutive frames to snap rendered segments to target segments when Smooth Movement is enabled.");
            public static readonly GUIContent reticle = new GUIContent("Reticle", "Stores the reticle that will appear at the end of the line when it is valid.");
            public static readonly GUIContent overrideInteractorLineLength = new GUIContent("Override Line Length", "Controls which source is used to determine the length of the line. Set to true to use the Line Length set by this behavior. Set to false have the length of the line determined by the interactor.");
            public static readonly GUIContent lineLength = new GUIContent("Line Length", "Controls the length of the line when overriding.");
            public static readonly GUIContent stopLineAtFirstRaycastHit = new GUIContent("Stop Line At First Raycast Hit", "Controls whether the line will always be cut short by this behavior at the first raycast hit, even when invalid.");
        }

        protected void OnEnable()
        {
            m_LineWidth = serializedObject.FindProperty("m_LineWidth");
            m_WidthCurve = serializedObject.FindProperty("m_WidthCurve");
            m_ValidColorGradient = serializedObject.FindProperty("m_ValidColorGradient");
            m_InvalidColorGradient = serializedObject.FindProperty("m_InvalidColorGradient");
            m_SmoothMovement = serializedObject.FindProperty("m_SmoothMovement");
            m_FollowTightness = serializedObject.FindProperty("m_FollowTightness");
            m_SnapThresholdDistance = serializedObject.FindProperty("m_SnapThresholdDistance");
            m_Reticle = serializedObject.FindProperty("m_Reticle");
            m_OverrideInteractorLineLength = serializedObject.FindProperty("m_OverrideInteractorLineLength");
            m_LineLength = serializedObject.FindProperty("m_LineLength");
            m_StopLineAtFirstRaycastHit = serializedObject.FindProperty("m_StopLineAtFirstRaycastHit");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRInteractorLineVisual)target), typeof(XRInteractorLineVisual), false);
            EditorGUI.EndDisabledGroup();

            EditorGUILayout.PropertyField(m_LineWidth, Tooltips.lineWidth);
            EditorGUILayout.PropertyField(m_WidthCurve, Tooltips.widthCurve);
            EditorGUILayout.PropertyField(m_ValidColorGradient, Tooltips.validColorGradient);
            EditorGUILayout.PropertyField(m_InvalidColorGradient, Tooltips.invalidColorGradient);

            EditorGUILayout.Space();

            // Override Line Length
            EditorGUILayout.PropertyField(m_OverrideInteractorLineLength, Tooltips.overrideInteractorLineLength);
            if (m_OverrideInteractorLineLength.boolValue)
            {
                EditorGUI.indentLevel++;
                EditorGUILayout.PropertyField(m_LineLength, Tooltips.lineLength);
                EditorGUI.indentLevel--;
            }

            EditorGUILayout.PropertyField(m_StopLineAtFirstRaycastHit, Tooltips.stopLineAtFirstRaycastHit);

            // Smooth Movement
            EditorGUILayout.PropertyField(m_SmoothMovement, Tooltips.smoothMovement);

            if (m_SmoothMovement.boolValue)
            {
                EditorGUI.indentLevel++;
                EditorGUILayout.PropertyField(m_FollowTightness, Tooltips.followTightness);
                EditorGUILayout.PropertyField(m_SnapThresholdDistance, Tooltips.snapThresholdDistance);
                EditorGUI.indentLevel--;
            }

            // Reticle
            EditorGUILayout.PropertyField(m_Reticle, Tooltips.reticle);
            serializedObject.ApplyModifiedProperties();
        }
    }
}
