using System.Linq;
using UnityEditor;
using UnityEngine;

namespace ViveHandTracking {

[CustomPropertyDrawer(typeof(NodeDistanceCondition))]
public class NodeDistanceConditionDrawer : PropertyDrawer {
  private static readonly string[] NodeNames = new string[] {
    "Wrist", "Thumb Tip", "Index Tip", "Middle Tip", "Ring Tip", "Pinky Tip"
  };
  private static readonly string[] LeftNodeNames = new string[] {
    "Left Wrist", "Left Thumb Tip", "Left Index Tip",
    "Left Middle Tip", "Left Ring Tip", "Left Pinky Tip"
  };
  private static readonly string[] RightNodeNames = new string[] {
    "Right Wrist", "Right Thumb Tip", "Right Index Tip",
    "Right Middle Tip", "Right Ring Tip", "Right Pinky Tip"
  };
  private static readonly int[] NodeIndex = new int[] {0, 4, 8, 12, 16, 20};
  private static readonly string[] ButtonText = new string[] {
    "< 2.5 cm", "> 5 cm", // single hand
    "< 10 cm", "> 20 cm",    // dual hand
  };

  public override void OnGUI(Rect position, SerializedProperty property, GUIContent label) {
    var attribute = fieldInfo.GetCustomAttributes(typeof(DualHandAttribute), true).FirstOrDefault() as DualHandAttribute;
    bool dualHand = attribute != null;

    EditorGUI.BeginProperty(position, label, property);
    position = EditorGUI.PrefixLabel(position, GUIUtility.GetControlID(FocusType.Passive), label);

    var indent = EditorGUI.indentLevel;
    EditorGUI.indentLevel = 0;

    var width = (position.width - 80) / 2;
    position.width = width;
    var prop = property.FindPropertyRelative("node1");
    prop.intValue = EditorGUI.IntPopup(position, "", prop.intValue, dualHand ? LeftNodeNames : NodeNames, NodeIndex);

    position.x += width + 5;
    prop = property.FindPropertyRelative("node2");
    prop.intValue = EditorGUI.IntPopup(position, "", prop.intValue, dualHand ? RightNodeNames : NodeNames, NodeIndex);

    position.x += width + 5;
    position.width = 70;
    prop = property.FindPropertyRelative("distance");

    if (prop.hasMultipleDifferentValues) {
      bool enabled = GUI.enabled;
      GUI.enabled = false;
      GUI.Button(position, "-");
      GUI.enabled = enabled;
    } else {
      var index = dualHand ? 2 : 0;
      if (GUI.Button(position, ButtonText[index + prop.intValue]))
        prop.intValue = 1 - prop.intValue;
    }

    EditorGUI.indentLevel = indent;
    EditorGUI.EndProperty();
  }
}

}
