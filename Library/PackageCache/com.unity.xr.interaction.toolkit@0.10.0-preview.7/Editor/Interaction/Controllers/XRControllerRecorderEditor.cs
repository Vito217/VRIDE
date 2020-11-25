using System.Collections.Generic;
using System.Linq;
using UnityEditor;

namespace UnityEngine.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRControllerRecorder)), CanEditMultipleObjects]
    public class XRControllerRecorderEditor : Editor
    {
        List<XRControllerRecorder> m_ControllerRecorders;

        static readonly GUIContent s_MixedValueContent = EditorGUIUtility.TrTextContent("\u2014", "Mixed Values");

        protected void OnEnable()
        {
            m_ControllerRecorders = targets.Cast<XRControllerRecorder>().ToList();
        }

        public override void OnInspectorGUI()
        {
            DrawDefaultInspector();

            // Show playback controls
            if (Application.isPlaying)
            {
                EditorGUILayout.BeginHorizontal(GUILayout.ExpandWidth(false));

                if (m_ControllerRecorders.All(controllerRecorder => controllerRecorder.isRecording))
                {
                    if (GUILayout.Button("Stop Recording"))
                        m_ControllerRecorders.ForEach(controllerRecorder => controllerRecorder.isRecording = false);
                }
                else if (m_ControllerRecorders.All(controllerRecorder => !controllerRecorder.isRecording))
                {
                    if (GUILayout.Button("Record Input"))
                        m_ControllerRecorders.ForEach(controllerRecorder => controllerRecorder.isRecording = true);
                }
                else
                {
                    EditorGUI.BeginDisabledGroup(true);
                    GUILayout.Button(s_MixedValueContent);
                    EditorGUI.EndDisabledGroup();
                }

                if (m_ControllerRecorders.All(controllerRecorder => controllerRecorder.isPlaying))
                {
                    if (GUILayout.Button("Stop"))
                        m_ControllerRecorders.ForEach(controllerRecorder => controllerRecorder.isPlaying = false);
                }
                else if (m_ControllerRecorders.All(controllerRecorder => !controllerRecorder.isPlaying))
                {
                    if (GUILayout.Button("Play"))
                        m_ControllerRecorders.ForEach(controllerRecorder => controllerRecorder.isPlaying = true);
                }
                else
                {
                    EditorGUI.BeginDisabledGroup(true);
                    GUILayout.Button(s_MixedValueContent);
                    EditorGUI.EndDisabledGroup();
                }

                EditorGUILayout.EndHorizontal();

                var currentTime = (float)((XRControllerRecorder)target).currentTime;
                var duration = (float)((XRControllerRecorder)target).duration;
                if (!serializedObject.isEditingMultipleObjects ||
                    m_ControllerRecorders.All(controllerRecorder => Mathf.Approximately((float)controllerRecorder.currentTime, currentTime)) &&
                    m_ControllerRecorders.All(controllerRecorder => Mathf.Approximately((float)controllerRecorder.duration, duration)))
                {
                    EditorGUI.BeginDisabledGroup(true);
                    EditorGUILayout.Slider(currentTime, 0f, duration);
                    EditorGUI.EndDisabledGroup();
                }
            }
        }
    }
}
