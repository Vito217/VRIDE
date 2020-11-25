using UnityEditor;

namespace UnityEngine.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRControllerRecording))]
    class XRControllerRecordingEditor : Editor
    {
        XRControllerRecording m_ControllerRecording;

        SerializedProperty m_Frames;

        protected void OnEnable()
        {
            m_ControllerRecording = (XRControllerRecording)target;
            m_Frames = serializedObject.FindProperty("m_Frames");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            if (GUILayout.Button("Clear Recording"))
                m_Frames.ClearArray();

            GUILayout.Label("Frames");
            GUILayout.BeginVertical();
            DisplayRecordingFrames();
            GUILayout.Space(5);
            GUILayout.EndVertical();

            serializedObject.ApplyModifiedProperties();
        }

        void DisplayRecordingFrames()
        {
            foreach (var frame in m_ControllerRecording.frames)
            {
                EditorGUILayout.BeginHorizontal();
                EditorGUILayout.FloatField((float)frame.time, GUILayout.ExpandWidth(true));
                EditorGUILayout.TextField(frame.position.ToString(), GUILayout.Width(120));
                EditorGUILayout.TextField(frame.rotation.ToString(), GUILayout.Width(160));
                EditorGUILayout.Toggle(frame.selectInteractionState.active, GUILayout.MaxWidth(14));
                EditorGUILayout.Toggle(frame.activateInteractionState.active, GUILayout.MaxWidth(14));
                EditorGUILayout.Toggle(frame.uiPressInteractionState.active, GUILayout.MaxWidth(14));
                EditorGUILayout.EndHorizontal();
            }
        }
    }
}