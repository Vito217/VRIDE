using UnityEngine.XR.Interaction.Toolkit;

namespace UnityEditor.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(TeleportationAnchor)), CanEditMultipleObjects]
    public class TeleportationAnchorEditor : Editor
    {
        SerializedProperty m_InteractionManager;
        SerializedProperty m_InteractionLayerMask;
        SerializedProperty m_Colliders;
        SerializedProperty m_CustomReticle;
        SerializedProperty m_TeleportAnchorTransform;

        SerializedProperty m_TeleportationProvider;
        SerializedProperty m_MatchOrientation;
        SerializedProperty m_TeleportTrigger;

        SerializedProperty m_OnFirstHoverEntered;
        SerializedProperty m_OnHoverEntered;
        SerializedProperty m_OnHoverExited;
        SerializedProperty m_OnLastHoverExited;
        SerializedProperty m_OnSelectEntered;
        SerializedProperty m_OnSelectExited;
        SerializedProperty m_OnSelectCanceled;
        SerializedProperty m_OnActivate;
        SerializedProperty m_OnDeactivate;

        protected void OnEnable()
        {
            m_InteractionManager = serializedObject.FindProperty("m_InteractionManager");
            m_InteractionLayerMask = serializedObject.FindProperty("m_InteractionLayerMask");
            m_Colliders = serializedObject.FindProperty("m_Colliders");
            m_CustomReticle = serializedObject.FindProperty("m_CustomReticle");
            m_TeleportAnchorTransform = serializedObject.FindProperty("m_TeleportAnchorTransform");

            m_TeleportationProvider = serializedObject.FindProperty("m_TeleportationProvider");
            m_MatchOrientation = serializedObject.FindProperty("m_MatchOrientation");
            m_TeleportTrigger = serializedObject.FindProperty("m_TeleportTrigger");

            m_OnFirstHoverEntered = serializedObject.FindProperty("m_OnFirstHoverEntered");
            m_OnHoverEntered = serializedObject.FindProperty("m_OnHoverEntered");
            m_OnHoverExited = serializedObject.FindProperty("m_OnHoverExited");
            m_OnLastHoverExited = serializedObject.FindProperty("m_OnLastHoverExited");
            m_OnSelectEntered = serializedObject.FindProperty("m_OnSelectEntered");
            m_OnSelectExited = serializedObject.FindProperty("m_OnSelectExited");
            m_OnSelectCanceled = serializedObject.FindProperty("m_OnSelectCanceled");
            m_OnActivate = serializedObject.FindProperty("m_OnActivate");
            m_OnDeactivate = serializedObject.FindProperty("m_OnDeactivate");

            // Set default expanded for some foldouts
            const string initializedKey = "XRI." + nameof(TeleportationAnchorEditor) + ".Initialized";
            if (!SessionState.GetBool(initializedKey, false))
            {
                SessionState.SetBool(initializedKey, true);
                m_MatchOrientation.isExpanded = true;
            }
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((TeleportationAnchor)target), typeof(TeleportationAnchor), false);
            EditorGUI.EndDisabledGroup();

            // Interaction Management
            EditorGUILayout.PropertyField(m_InteractionManager);
            EditorGUILayout.PropertyField(m_InteractionLayerMask);
            EditorGUILayout.PropertyField(m_Colliders);
            EditorGUILayout.PropertyField(m_CustomReticle);
            EditorGUILayout.PropertyField(m_TeleportAnchorTransform);

            EditorGUILayout.Space();

            // Teleportation
            m_MatchOrientation.isExpanded = EditorGUILayout.Foldout(m_MatchOrientation.isExpanded, EditorGUIUtility.TrTempContent("Teleportation Configuration"), true);
            if (m_MatchOrientation.isExpanded)
            {
                EditorGUI.indentLevel++;

                EditorGUILayout.PropertyField(m_MatchOrientation);
                EditorGUILayout.PropertyField(m_TeleportTrigger);
                EditorGUILayout.PropertyField(m_TeleportationProvider);

                EditorGUI.indentLevel--;
            }

            EditorGUILayout.Space();

            // Interactable Events
            m_OnFirstHoverEntered.isExpanded = EditorGUILayout.Foldout(m_OnFirstHoverEntered.isExpanded, EditorGUIUtility.TrTempContent("Interactable Events"), true);
            if (m_OnFirstHoverEntered.isExpanded)
            {
                EditorGUILayout.PropertyField(m_OnFirstHoverEntered);
                EditorGUILayout.PropertyField(m_OnLastHoverExited);
                EditorGUILayout.PropertyField(m_OnHoverEntered);
                EditorGUILayout.PropertyField(m_OnHoverExited);
                EditorGUILayout.PropertyField(m_OnSelectEntered);
                EditorGUILayout.PropertyField(m_OnSelectExited);
                EditorGUILayout.PropertyField(m_OnSelectCanceled);
                EditorGUILayout.PropertyField(m_OnActivate);
                EditorGUILayout.PropertyField(m_OnDeactivate);
            }

            serializedObject.ApplyModifiedProperties();
        }
    }
}
