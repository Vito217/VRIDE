using UnityEditor;
using UnityEditor.UI;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;
using UnityEngine.Scripting.APIUpdating;

namespace Unity.VectorGraphics
{
    /// <summary>
    /// Editor class used to edit UI Sprites.
    /// </summary>
    [CustomEditor(typeof(SVGImage), isFallback = true)]
    [MovedFrom("")]
    [CanEditMultipleObjects]
    /// <summary>
    ///   Custom Editor for the Image Component.
    ///   Extend this class to write a custom editor for an Image-derived component.
    /// </summary>
    public class SVGImageEditor : GraphicEditor
    {
        SerializedProperty m_Sprite;
        SerializedProperty m_PreserveAspect;
        GUIContent m_SpriteContent;
        GUIContent m_PreserveAspectContent;

        /// <summary>Enables the editor</summary>
        protected override void OnEnable()
        {
            base.OnEnable();

            m_SpriteContent = new GUIContent("Source SVG Image");
            m_Sprite = serializedObject.FindProperty("m_Sprite");

            m_PreserveAspectContent = new GUIContent("Preserve Aspect");
            m_PreserveAspect = serializedObject.FindProperty("m_PreserveAspect");
        }

        /// <summary>Draws the editor</summary>
        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUILayout.PropertyField(m_Sprite, m_SpriteContent);
            AppearanceControlsGUI();
            RaycastControlsGUI();       
            EditorGUILayout.PropertyField(m_PreserveAspect, m_PreserveAspectContent);

            serializedObject.ApplyModifiedProperties();
        }

        private static Canvas CreateCanvasGameObject()
        {
            Canvas canvas;
            // Create new Canvas since none exists in the scene.
            GameObject canvasObject = new GameObject("Canvas");
            canvas = canvasObject.AddComponent<Canvas>();
            canvas.renderMode = RenderMode.ScreenSpaceOverlay;

            // Add a Graphic Raycaster Component as well
            canvas.gameObject.AddComponent<GraphicRaycaster>();

            Undo.RegisterCreatedObjectUndo(canvasObject, "Create " + canvasObject.name);

            return canvas;
        }

        private static GameObject GetOrCreateCanvasGameObject()
        {
            // Check if there is a Canvas in the scene
            Canvas canvas = Object.FindObjectOfType<Canvas>();
            if (canvas == null)
            {
                canvas = CreateCanvasGameObject();
            }

            return canvas.gameObject;
        }

        private static void PlaceUIElementRoot(GameObject element, MenuCommand menuCommand)
        {
            GameObject parent = menuCommand.context as GameObject;
            if (parent == null)
            {
                parent = GetOrCreateCanvasGameObject();
            }
            if (parent.GetComponentInParent<Canvas>() == null)
            {
                // Create canvas under context GameObject,
                // and make that be the parent which UI element is added under.
                Canvas canvas = CreateCanvasGameObject();
                canvas.transform.SetParent(parent.transform, false);
                parent = canvas.gameObject;
            }

            // Setting the element to be a child of an element already in the scene should
            // be sufficient to also move the element to that scene.
            // However, it seems the element needs to be already in its destination scene when the
            // RegisterCreatedObjectUndo is performed; otherwise the scene it was created in is dirtied.
            SceneManager.MoveGameObjectToScene(element, parent.scene);

            Undo.RegisterCreatedObjectUndo(element, "Create " + element.name);

            if (element.transform.parent == null)
            {
                Undo.SetTransformParent(element.transform, parent.transform, "Parent " + element.name);
            }
    #if UNITY_2018_3_OR_NEWER
            GameObjectUtility.EnsureUniqueNameForSibling(element);
    #else
            element.name = GameObjectUtility.GetUniqueNameForSibling(parent.transform, element.name);
    #endif
            // We have to fix up the undo name since the name of the object was only known after reparenting it.
            Undo.SetCurrentGroupName("Create " + element.name);

            GameObjectUtility.SetParentAndAlign(element, parent);

            Selection.activeGameObject = element;
        }

        /// <summary>Adds the SVG Image menu item.</summary>
        /// <param name="menuCommand">The menu command into which to insert the menu item.</param>
        [MenuItem("GameObject/UI/SVG Image", false, 2002)]
        static public void AddSVGImage(MenuCommand menuCommand)
        {
            GameObject go = new GameObject("SVG Image");
            RectTransform rectTransform = go.AddComponent<RectTransform>();
            rectTransform.sizeDelta = new Vector2(100f, 100f);
            go.AddComponent<SVGImage>();
            PlaceUIElementRoot(go, menuCommand);
        }
    }
}