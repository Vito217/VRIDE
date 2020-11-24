using UnityEngine;

#if UNITY_EDITOR

using System;
using System.Collections.Generic;
using System.Linq;
using UnityEditor;

namespace ViveHandTracking {

[CustomPropertyDrawer(typeof(EnumFlagsAttribute))]
class EnumFlagsAttributeDrawer : PropertyDrawer {
  private List<int> enumValues = null;
  private int everything = -1;

  public override void OnGUI(Rect position, SerializedProperty property, GUIContent label) {
    if (enumValues == null)
      Setup(property);

    if (property.hasMultipleDifferentValues) {
      EditorGUI.PropertyField(position, property, label);
      return;
    }

    var attribute = fieldInfo.GetCustomAttributes(typeof(TooltipAttribute), true).FirstOrDefault() as TooltipAttribute;
    if (attribute != null)
      label.tooltip = attribute.tooltip;
    property.intValue = Mask2Enum(EditorGUI.MaskField(position, label, Enum2Mask(property.intValue),
                                  property.enumDisplayNames));
  }

  private void Setup(SerializedProperty property) {
    enumValues = new List<int>();
    everything = 0;
    foreach (var name in property.enumNames) {
      int v = (int)Enum.Parse(fieldInfo.FieldType, name);
      enumValues.Add(v);
      everything += v;
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

#endif

namespace ViveHandTracking {

class EnumFlagsAttribute : PropertyAttribute {
  public EnumFlagsAttribute() {}
}

}
