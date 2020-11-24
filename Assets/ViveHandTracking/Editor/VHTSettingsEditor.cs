using System;
using System.Linq;
using UnityEngine;
using UnityEditor;
using UnityEditorInternal;

namespace ViveHandTracking {

[CustomEditor(typeof(VHTSettings))]
class VHTSettingsEditor : Editor {
  VHTSettingsDrawer drawer = null;

  void OnEnable() {
    drawer = new VHTSettingsDrawer(serializedObject);
  }

  public override void OnInspectorGUI() {
    drawer.Draw();
  }
}

class VHTSettingsDrawer {
  ReorderableList list = null;
  SerializedProperty enginesProp;
  SerializedObject serializedObject;

  public VHTSettingsDrawer(SerializedObject serializedObject) {
    this.serializedObject = serializedObject;
    enginesProp = serializedObject.FindProperty("Engines");
    var target = serializedObject.targetObject as VHTSettings;
    list = new ReorderableList(serializedObject, enginesProp, true, true, true, true);
    list.drawHeaderCallback = (Rect rect) => {
      EditorGUI.LabelField(rect, "Detection Engines");
    };
    list.drawElementCallback = (Rect rect, int index, bool isActive, bool isFocused) => {
      var element = enginesProp.GetArrayElementAtIndex(index).objectReferenceValue;
      rect.y += 2;
      rect.height = EditorGUIUtility.singleLineHeight;
      EditorGUI.LabelField(rect, element.GetType().Name, (element as HandTrackingEngine).Description());
    };
    list.onCanRemoveCallback = (ReorderableList l) => {
      return l.count > 1 && (l.index < 0 ||
                             target.Engines[l.index].GetType() != typeof(ViveHandTrackingEngine));
    };
    list.onRemoveCallback = (ReorderableList l) => {
      var element = enginesProp.GetArrayElementAtIndex(l.index).objectReferenceValue;
      l.serializedProperty.DeleteArrayElementAtIndex(l.index);
      UnityEngine.Object.DestroyImmediate(element, true);
      for (int i = l.index + 1; i < l.serializedProperty.arraySize; i++)
        l.serializedProperty.MoveArrayElement(i, i - 1);
      l.serializedProperty.arraySize--;
      serializedObject.ApplyModifiedProperties();
      AssetDatabase.SaveAssets();
    };
    list.onAddDropdownCallback = (Rect buttonRect, ReorderableList l) => {
      var menu = new GenericMenu();
      var types = AppDomain.CurrentDomain.GetAssemblies()
                  .SelectMany(a => a.GetTypes())
                  .Where(t => t.IsSubclassOf(typeof(HandTrackingEngine)));
      var currentTypes = target.Engines.Where(e => e != null).Select(e => e.GetType()).ToList();
      foreach (var t in types) {
        if (currentTypes.Contains(t))
          menu.AddDisabledItem(new GUIContent(t.Name));
        else
          menu.AddItem(new GUIContent(t.Name), false, OnAddNewItem, t);
      }
      menu.ShowAsContext();
    };
  }

  public void Draw() {
    serializedObject.Update();
    list.DoLayoutList();
    EditorGUILayout.HelpBox("GestureProvider attempts to start detection using engines from top to bottom, " +
                            "if engine is supported on current platform.",
                            MessageType.Info);
#if VIVEHANDTRACKING_WAVEVR_HAND
    bool hasWaveVRHand = (serializedObject.targetObject as VHTSettings).Engines
                         .Any(t => t.GetType() == typeof(WaveVRHandEngine));
    if (hasWaveVRHand)
      EditorGUILayout.HelpBox("WaveVRHandEngine must use gradle build system to avoid kotlin.Any not found error.",
                              MessageType.Warning);
#endif
    serializedObject.ApplyModifiedProperties();
    GUILayout.Space(20);
    if (GUILayout.Button("Show Help"))
      Application.OpenURL("https://hub.vive.com/storage/tracking/unity/engine.html");
  }

  void OnAddNewItem(object item) {
    var index = enginesProp.arraySize;
    enginesProp.arraySize++;
    list.index = index;
    var element = enginesProp.GetArrayElementAtIndex(index);
    var engine = ScriptableObject.CreateInstance(item as Type);
    engine.name = (item as Type).Name;
    var target = serializedObject.targetObject as VHTSettings;
    AssetDatabase.AddObjectToAsset(engine, target);
    element.objectReferenceValue = engine;
    enginesProp.serializedObject.ApplyModifiedProperties();
  }
}

}
