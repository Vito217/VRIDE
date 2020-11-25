using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

namespace UnityEditor.XR.Interaction.Toolkit
{
    [CustomEditor(typeof(XRSocketInteractor)), CanEditMultipleObjects]
    public class XRSocketInteractorEditor : Editor
    {
        SerializedProperty m_InteractionManager;
        SerializedProperty m_InteractionLayerMask;
        SerializedProperty m_AttachTransform;
        SerializedProperty m_StartingSelectedInteractable;

        SerializedProperty m_ShowInteractableHoverMeshes;
        SerializedProperty m_InteractableHoverMeshMaterial;
        SerializedProperty m_InteractableCantHoverMeshMaterial;
        SerializedProperty m_SocketActive;
        SerializedProperty m_InteractableHoverScale;

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

            public static readonly GUIContent showInteractableHoverMeshes = new GUIContent("Show Interactable Hover Meshes", "Show interactable's meshes at socket's attach point on hover.");
            public static readonly GUIContent interactableHoverMeshMaterial = new GUIContent("Interactable Hover Mesh Material", "Material used for rendering interactable meshes on hover (a default material will be created if none is supplied).");
            public static readonly GUIContent interactableCantHoverMeshMaterial = new GUIContent("Interactable Cant Hover Mesh Material", "Material used for rendering interactable meshes on hover when there is already a selected object in the socket (a default material will be created if none is supplied).");
            public static readonly GUIContent socketActive = new GUIContent("Socket Active", "Turn socket interaction on/off.");
            public static readonly GUIContent interactableHoverScale = new GUIContent("Interactable Hover Scale", "Scale at which to render hovered interactable.");
        }

        protected void OnEnable()
        {
            m_InteractionManager = serializedObject.FindProperty("m_InteractionManager");
            m_InteractionLayerMask = serializedObject.FindProperty("m_InteractionLayerMask");
            m_AttachTransform = serializedObject.FindProperty("m_AttachTransform");
            m_StartingSelectedInteractable = serializedObject.FindProperty("m_StartingSelectedInteractable");

            m_ShowInteractableHoverMeshes = serializedObject.FindProperty("m_ShowInteractableHoverMeshes");
            m_InteractableHoverMeshMaterial = serializedObject.FindProperty("m_InteractableHoverMeshMaterial");
            m_InteractableCantHoverMeshMaterial = serializedObject.FindProperty("m_InteractableCantHoverMeshMaterial");
            m_SocketActive = serializedObject.FindProperty("m_SocketActive");
            m_InteractableHoverScale = serializedObject.FindProperty("m_InteractableHoverScale");

            m_OnHoverEntered = serializedObject.FindProperty("m_OnHoverEntered");
            m_OnHoverExited = serializedObject.FindProperty("m_OnHoverExited");
            m_OnSelectEntered = serializedObject.FindProperty("m_OnSelectEntered");
            m_OnSelectExited = serializedObject.FindProperty("m_OnSelectExited");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUI.BeginDisabledGroup(true);
            EditorGUILayout.ObjectField(EditorGUIUtility.TrTempContent("Script"), MonoScript.FromMonoBehaviour((XRSocketInteractor)target), typeof(XRSocketInteractor), false);
            EditorGUI.EndDisabledGroup();

            EditorGUILayout.PropertyField(m_InteractionManager, Tooltips.interactionManager);
            EditorGUILayout.PropertyField(m_InteractionLayerMask, Tooltips.interactionLayerMask);
            EditorGUILayout.PropertyField(m_AttachTransform, Tooltips.attachTransform);
            EditorGUILayout.PropertyField(m_StartingSelectedInteractable, Tooltips.startingSelectedInteractable);

            EditorGUILayout.PropertyField(m_ShowInteractableHoverMeshes, Tooltips.showInteractableHoverMeshes);
            EditorGUI.indentLevel++;
            if (m_ShowInteractableHoverMeshes.boolValue)
            {
                EditorGUILayout.PropertyField(m_InteractableHoverMeshMaterial, Tooltips.interactableHoverMeshMaterial);
                EditorGUILayout.PropertyField(m_InteractableCantHoverMeshMaterial, Tooltips.interactableCantHoverMeshMaterial);
            }
            EditorGUI.indentLevel--;
            EditorGUILayout.PropertyField(m_SocketActive, Tooltips.socketActive);
            EditorGUILayout.PropertyField(m_InteractableHoverScale, Tooltips.interactableHoverScale);

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
