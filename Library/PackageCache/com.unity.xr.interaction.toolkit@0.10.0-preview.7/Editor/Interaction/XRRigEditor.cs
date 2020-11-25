using UnityEditor;

namespace UnityEngine.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRRig)), CanEditMultipleObjects]
    public class XRRigEditor : Editor
    {
        SerializedProperty m_RigBaseGameObject;
        SerializedProperty m_CameraFloorOffsetObject;
        SerializedProperty m_CameraGameObject;
#if UNITY_2019_3_OR_NEWER
        SerializedProperty m_TrackingOriginMode;
#else
        SerializedProperty m_TrackingSpace;
#endif
        SerializedProperty m_CameraYOffset;

        protected void OnEnable()
        {
            m_RigBaseGameObject = serializedObject.FindProperty("m_RigBaseGameObject");
            m_CameraFloorOffsetObject = serializedObject.FindProperty("m_CameraFloorOffsetObject");
            m_CameraGameObject = serializedObject.FindProperty("m_CameraGameObject");
#if UNITY_2019_3_OR_NEWER
            m_TrackingOriginMode = serializedObject.FindProperty("m_TrackingOriginMode");
#else
            m_TrackingSpace = serializedObject.FindProperty("m_TrackingSpace");
#endif
            m_CameraYOffset = serializedObject.FindProperty("m_CameraYOffset");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRRig)target), typeof(XRRig), false);
            EditorGUI.EndDisabledGroup();

            EditorGUILayout.PropertyField(m_RigBaseGameObject);
            EditorGUILayout.PropertyField(m_CameraFloorOffsetObject);
            EditorGUILayout.PropertyField(m_CameraGameObject);

#if UNITY_2019_3_OR_NEWER
            EditorGUILayout.PropertyField(m_TrackingOriginMode);
            bool showCameraYOffset = m_TrackingOriginMode.enumValueIndex == (int)TrackingOriginModeFlags.Device;
#else
            EditorGUILayout.PropertyField(m_TrackingSpace);
            bool showCameraYOffset = m_TrackingSpace.enumValueIndex == (int)TrackingSpaceType.Stationary;
#endif
            if (showCameraYOffset)
            {
                EditorGUI.indentLevel++;
                EditorGUILayout.PropertyField(m_CameraYOffset);
                EditorGUI.indentLevel--;
            }

            serializedObject.ApplyModifiedProperties();
        }
    }
}   
