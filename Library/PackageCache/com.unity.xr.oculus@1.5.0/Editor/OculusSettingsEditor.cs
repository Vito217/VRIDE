using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using Unity.XR.Oculus;

namespace Unity.XR.Oculus.Editor
{
    [CustomEditor(typeof(OculusSettings))]
    public class OculusSettingsEditor : UnityEditor.Editor
    {
        private const string kSharedDepthBuffer = "SharedDepthBuffer";
        private const string kDashSupport = "DashSupport";
        private const string kStereoRenderingModeDesktop = "m_StereoRenderingModeDesktop";
        private const string kStereoRenderingModeAndroid = "m_StereoRenderingModeAndroid";
        private const string kV2Signing = "V2Signing";
        private const string kLowOverheadMode = "LowOverheadMode";
        private const string kProtectedContext = "ProtectedContext";
        private const string kFocusAware = "FocusAware";
        private const string kOptimizeBufferDiscards = "OptimizeBufferDiscards";

        static GUIContent s_SharedDepthBufferLabel = EditorGUIUtility.TrTextContent("Shared Depth Buffer");
        static GUIContent s_DashSupportLabel = EditorGUIUtility.TrTextContent("Dash Support");
        static GUIContent s_StereoRenderingModeLabel = EditorGUIUtility.TrTextContent("Stereo Rendering Mode");
        static GUIContent s_V2SigningLabel = EditorGUIUtility.TrTextContent("V2 Signing (Quest)");
        static GUIContent s_LowOverheadModeLabel = EditorGUIUtility.TrTextContent("Low Overhead Mode (GLES)");
        static GUIContent s_ProtectedContextLabel = EditorGUIUtility.TrTextContent("Protected Context");
        static GUIContent s_FocusAwareLabel = EditorGUIUtility.TrTextContent("Focus Aware");
        static GUIContent s_OptimizeBufferDiscardsLabel = EditorGUIUtility.TrTextContent("Optimize Buffer Discards (Vulkan)");

        private SerializedProperty m_SharedDepthBuffer;
        private SerializedProperty m_DashSupport;
        private SerializedProperty m_StereoRenderingModeDesktop;
        private SerializedProperty m_StereoRenderingModeAndroid;
        private SerializedProperty m_V2Signing;
        private SerializedProperty m_LowOverheadMode;
        private SerializedProperty m_ProtectedContext;
        private SerializedProperty m_FocusAware;
        private SerializedProperty m_OptimizeBufferDiscards;

        public override void OnInspectorGUI()
        {
            if (serializedObject == null || serializedObject.targetObject == null)
                return;

            if (m_SharedDepthBuffer == null) m_SharedDepthBuffer = serializedObject.FindProperty(kSharedDepthBuffer);
            if (m_DashSupport == null) m_DashSupport = serializedObject.FindProperty(kDashSupport);
            if (m_StereoRenderingModeDesktop == null) m_StereoRenderingModeDesktop = serializedObject.FindProperty(kStereoRenderingModeDesktop);
            if (m_StereoRenderingModeAndroid == null) m_StereoRenderingModeAndroid = serializedObject.FindProperty(kStereoRenderingModeAndroid);
            if (m_V2Signing == null) m_V2Signing = serializedObject.FindProperty(kV2Signing);
            if (m_LowOverheadMode == null) m_LowOverheadMode = serializedObject.FindProperty(kLowOverheadMode);
            if (m_ProtectedContext == null) m_ProtectedContext = serializedObject.FindProperty(kProtectedContext);
            if (m_FocusAware == null) m_FocusAware = serializedObject.FindProperty(kFocusAware);
            if (m_OptimizeBufferDiscards == null) m_OptimizeBufferDiscards = serializedObject.FindProperty(kOptimizeBufferDiscards);

            serializedObject.Update();

            EditorGUIUtility.labelWidth = 220.0f;

            BuildTargetGroup selectedBuildTargetGroup = EditorGUILayout.BeginBuildTargetSelectionGrouping();
            EditorGUILayout.Space();

            EditorGUILayout.BeginVertical(GUILayout.ExpandWidth(true));
            if (EditorApplication.isPlayingOrWillChangePlaymode)
            {
                EditorGUILayout.HelpBox("Oculus settings cannnot be changed when the editor is in play mode.", MessageType.Info);
                EditorGUILayout.Space();
            }
            EditorGUI.BeginDisabledGroup(EditorApplication.isPlayingOrWillChangePlaymode);
            if (selectedBuildTargetGroup == BuildTargetGroup.Standalone)
            {
                EditorGUILayout.PropertyField(m_StereoRenderingModeDesktop, s_StereoRenderingModeLabel);
                EditorGUILayout.PropertyField(m_SharedDepthBuffer, s_SharedDepthBufferLabel);
                EditorGUILayout.PropertyField(m_DashSupport, s_DashSupportLabel);
            }
            else if(selectedBuildTargetGroup == BuildTargetGroup.Android)
            {
                EditorGUILayout.PropertyField(m_StereoRenderingModeAndroid, s_StereoRenderingModeLabel);
                EditorGUILayout.PropertyField(m_LowOverheadMode, s_LowOverheadModeLabel);
                EditorGUILayout.PropertyField(m_ProtectedContext, s_ProtectedContextLabel);
                EditorGUILayout.PropertyField(m_OptimizeBufferDiscards, s_OptimizeBufferDiscardsLabel);
                EditorGUILayout.PropertyField(m_FocusAware, s_FocusAwareLabel);
                EditorGUILayout.PropertyField(m_V2Signing, s_V2SigningLabel);
            }
            EditorGUI.EndDisabledGroup();
            EditorGUILayout.EndVertical();
            EditorGUILayout.EndBuildTargetSelectionGrouping();

            serializedObject.ApplyModifiedProperties();

            EditorGUIUtility.labelWidth = 0.0f;
        }
    }
}
