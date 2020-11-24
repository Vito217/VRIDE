using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using UnityEditor;
using UnityEditor.AnimatedValues;
using UnityEditorInternal;

namespace ViveHandTracking {

class GestureList {
  SerializedProperty prop;
  ReorderableList list;
  int count;

  internal GestureList(int count, bool SingleHand, SerializedProperty prop) {
    this.prop = prop;
    this.count = count;

    string header = (SingleHand ? "Single" : "Dual") + " Hand Custom Gestures";
    string element = SingleHand ? "Custom Gesture " : "Dual Hand Gesture ";

    list = new ReorderableList(prop.serializedObject, prop, true, true, true, true);
    list.drawHeaderCallback = (Rect rect) => {
      EditorGUI.LabelField(rect, header);
    };
    list.drawElementCallback = (Rect rect, int index, bool isActive, bool isFocused) => {
      var elementProp = prop.GetArrayElementAtIndex(index);
      rect.y += 2;
      rect.height = EditorGUIUtility.singleLineHeight;
      string name = element + (index + 1).ToString();
      if (index >= count)
        name = "Extra " + element + (index + 1 - count).ToString();
      EditorGUI.PropertyField(rect, elementProp, new GUIContent(name));
    };
    list.onRemoveCallback = (ReorderableList l) => {
      l.serializedProperty.DeleteArrayElementAtIndex(l.index);
      for (int i = l.index + 1; i < l.serializedProperty.arraySize; i++)
        l.serializedProperty.MoveArrayElement(i, i - 1);
      l.serializedProperty.arraySize--;
    };
  }

  internal void Draw() {
    list.DoLayoutList();
    if (prop.arraySize > count)
      EditorGUILayout.HelpBox("Extra gestures are not supported in HandStateChecker, you need to check gesture manually using CustomGestureProvider",
                              MessageType.Warning);
  }
}

[CustomEditor(typeof(CustomGestureProvider))]
class CustomGestureProviderEditor : Editor {
  SerializedProperty singleProp, dualProp;
  GestureList singleList = null, dualList = null;

  void OnEnable() {
    singleProp = serializedObject.FindProperty("SingleHandCustomGestures");
    dualProp = serializedObject.FindProperty("DualHandCustomGestures");

    singleList = new GestureList(CustomGestureProvider.MaxSingleHandCustomStates, true, singleProp);
    dualList = new GestureList(CustomGestureProvider.MaxDualHandCustomStates, false, dualProp);
  }

  public override void OnInspectorGUI() {
    serializedObject.Update();

    GUI.enabled = false;
    SerializedProperty prop = serializedObject.FindProperty("m_Script");
    EditorGUILayout.PropertyField(prop, true, new GUILayoutOption[0]);
    GUI.enabled = true;

    singleList.Draw();
    dualList.Draw();
    serializedObject.ApplyModifiedProperties();
  }
}

}
