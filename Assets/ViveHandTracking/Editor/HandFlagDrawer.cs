using System;
using System.Collections.Generic;
using System.Linq;
using UnityEditor;
using UnityEngine;

namespace ViveHandTracking {

class CGPWrapper {
  public CustomGestureProvider CGP = null;

  public CGPWrapper(CustomGestureProvider CGP) {
    this.CGP = CGP;
  }
}

[CustomPropertyDrawer(typeof(HandFlag))]
public class HandFlagDrawer : PropertyDrawer {
  private List<int> enumValues = null;
  private List<string> enumNames = null;
  private int everything = -1;

  public override void OnGUI(Rect position, SerializedProperty property, GUIContent label) {
    if (property.hasMultipleDifferentValues) {
      EditorGUI.PropertyField(position, property, label);
      return;
    }

    Setup(property);
    var attribute = fieldInfo.GetCustomAttributes(typeof(TooltipAttribute), true).FirstOrDefault() as TooltipAttribute;
    if (attribute != null)
      label.tooltip = attribute.tooltip;
    property.intValue = Mask2Enum(EditorGUI.MaskField(position, label, Enum2Mask(property.intValue),
                                  enumNames.ToArray()));
  }

  private static CGPWrapper GetCustomGestureProvider(SerializedProperty property) {
    if (property == null)
      return null;
    var comp = property.serializedObject.targetObject as Component;
    if (comp == null)
      return null;
    var scene = comp.gameObject.scene;
    if (scene == null || !scene.isLoaded)
      return null;
    return new CGPWrapper(scene.GetRootGameObjects()
                          .SelectMany(go => go.GetComponentsInChildren<CustomGestureProvider>())
                          .FirstOrDefault());
  }

  private void Setup(SerializedProperty property) {
    var wrapper = GetCustomGestureProvider(property);
    Func<int, bool> filter = null;
    if (wrapper != null) {
      if (wrapper.CGP == null)
        filter = v => v >= (int)HandFlag.CustomGesture1;
      else {
        int single = Math.Min(wrapper.CGP.SingleHandCustomGestures.Count, CustomGestureProvider.MaxSingleHandCustomStates);
        int dual = Math.Min(wrapper.CGP.DualHandCustomGestures.Count, CustomGestureProvider.MaxDualHandCustomStates);
        int singleStart = ((int)HandFlag.CustomGesture1 >> 1) << single;
        int dualStart = ((int)HandFlag.DualGesture1 >> 1) << dual;
        filter = v => (v > singleStart && v < (int)HandFlag.DualGesture1) || (v > dualStart);
      }
    }

    if (enumValues == null) {
      enumValues = new List<int>();
      enumNames = new List<string>();
    } else {
      enumValues.Clear();
      enumNames.Clear();
    }
    everything = 0;
    foreach (var name in property.enumNames) {
      int v = (int)Enum.Parse(fieldInfo.FieldType, name);
      if (filter != null && filter(v))
        continue;
      everything += v;
      enumValues.Add(v);
      var displayName = name;
      if (wrapper != null && wrapper.CGP != null) {
        if (v >= (int)HandFlag.DualGesture1) {
          v = (int)(Math.Log(v / (int)HandFlag.DualGesture1, 2) + 0.5);
          displayName = wrapper.CGP.DualHandCustomGestures[v].name;
          displayName += " (" + name + ")";
        } else if (v >= (int)HandFlag.CustomGesture1) {
          v = (int)(Math.Log(v / (int)HandFlag.CustomGesture1, 2) + 0.5);
          displayName = wrapper.CGP.SingleHandCustomGestures[v].name;
          displayName += " (" + name + ")";
        }
      }
      enumNames.Add(displayName);
    }
  }

  private int Enum2Mask(int enumValue) {
    if (enumValue == everything)
      return -1;
    int mask = 1;
    int maskValue = 0;
    foreach (var v in enumValues) {
      if ((enumValue & v) == v)
        maskValue += mask;
      mask <<= 1;
    }
    return maskValue;
  }

  private int Mask2Enum(int maskValue) {
    int mask = 1;
    int enumValue = 0;
    foreach (var v in enumValues) {
      if ((maskValue & mask) == mask)
        enumValue += v;
      mask <<= 1;
    }
    return enumValue;
  }
}

}
