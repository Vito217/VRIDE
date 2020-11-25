using System;

using UnityEngine;
using UnityEngine.XR.WindowsMR;

using UnityEditor;
using UnityEditor.XR.Management;

namespace UnityEditor.XR.WindowsMR
{
    [CustomEditor(typeof(WindowsMRPackageSettings))]
    /// <summary>Custom editor settings support for this XR Plugin.</summary>
    public class SettingsEditor : UnityEditor.Editor
    {
        const string k_DepthBufferFormat = "DepthBufferFormat";
        const string k_SharedDepthBuffer = "UseSharedDepthBuffer";
        const string k_ForcePrimaryWindowHolographic = "UsePrimaryWindowForDisplay";
        const string k_HolographicRemoting = "HolographicRemoting";

        static GUIContent s_DepthBufferFormatLabel = new GUIContent("Depth Buffer Format");
        static GUIContent s_SharedDepthBufferLabel = new GUIContent("Shared Depth Buffer");
        static GUIContent s_HolographicRemotingLabel = new GUIContent("Holographic Remoting");
        static GUIContent s_ForcePrimaryWindowHologrpahicLabel = new GUIContent("Use Primary Window");
        static GUIContent s_ShowBuildSettingsLabel = new GUIContent("Build Settings");
        static GUIContent s_ShowRuntimeSettingsLabel = new GUIContent("Runtime Settings");

        bool m_ShowBuildSettings = true;
        bool m_ShowRuntimeSettings = true;

        public override void OnInspectorGUI()
        {
            if (serializedObject == null || serializedObject.targetObject == null)
                return;

            WindowsMRPackageSettings settings = serializedObject.targetObject as WindowsMRPackageSettings;

            BuildTargetGroup buildTargetGroup = EditorGUILayout.BeginBuildTargetSelectionGrouping();

            if (buildTargetGroup == BuildTargetGroup.Standalone || buildTargetGroup == BuildTargetGroup.WSA)
            {
                serializedObject.Update();

                if (buildTargetGroup == BuildTargetGroup.WSA)
                {
                    m_ShowBuildSettings = EditorGUILayout.Foldout(m_ShowBuildSettings, s_ShowBuildSettingsLabel);
                    if (m_ShowBuildSettings)
                    {
                        var serializedSettingsObject = new SerializedObject(settings.GetBuildSettingsForBuildTargetGroup(buildTargetGroup));
                        serializedSettingsObject.Update();

                        SerializedProperty forcePrimaryWindowHologrpahic = serializedSettingsObject.FindProperty(k_ForcePrimaryWindowHolographic);
                        SerializedProperty holographicRemoting = serializedSettingsObject.FindProperty(k_HolographicRemoting);

                        EditorGUI.indentLevel++;
                        EditorGUILayout.PropertyField(forcePrimaryWindowHologrpahic, s_ForcePrimaryWindowHologrpahicLabel);
                        EditorGUILayout.Space();
                        EditorGUILayout.PropertyField(holographicRemoting, s_HolographicRemotingLabel);
                        EditorGUI.indentLevel--;
                        serializedSettingsObject.ApplyModifiedProperties();
                    }

                }

                EditorGUILayout.Space();

                if (buildTargetGroup == BuildTargetGroup.WSA)
                    m_ShowRuntimeSettings = EditorGUILayout.Foldout(m_ShowRuntimeSettings, s_ShowRuntimeSettingsLabel);
                else
                    m_ShowRuntimeSettings = true;

                if (m_ShowRuntimeSettings)
                {
                    var serializedSettingsObject = new SerializedObject(settings.GetSettingsForBuildTargetGroup(buildTargetGroup));
                    serializedSettingsObject.Update();

                    SerializedProperty depthBufferFormat = serializedSettingsObject.FindProperty(k_DepthBufferFormat);
                    SerializedProperty sharedDepthBuffer = serializedSettingsObject.FindProperty(k_SharedDepthBuffer);

                    EditorGUI.indentLevel++;
                    EditorGUILayout.PropertyField(depthBufferFormat, s_DepthBufferFormatLabel);
                    EditorGUILayout.Space();
                    EditorGUILayout.PropertyField(sharedDepthBuffer, s_SharedDepthBufferLabel);
                    EditorGUI.indentLevel--;
                    serializedSettingsObject.ApplyModifiedProperties();
                }
                serializedObject.ApplyModifiedProperties();

            }
            else
            {
                EditorGUILayout.HelpBox("Settings for this package are unsupported for this target platform.", MessageType.Info);
            }
            EditorGUILayout.EndBuildTargetSelectionGrouping();

        }
    }
}
