using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using UnityEditor;
using UnityEditor.AnimatedValues;

namespace ViveHandTracking {

[CustomEditor(typeof(ModelRenderer))]
[CanEditMultipleObjects]
class ModelRendererEditor : Editor {
  private static readonly string[] names = new string[] {
    "Wrist",
    "Thumb root", "Thumb joint 1", "Thumb joint 2", "Thumb top",
    "Index root", "Index joint 1", "Index joint 2", "Index top",
    "Middle root", "Middle joint 1", "Middle joint 2", "Middle top",
    "Ring root", "Ring joint 1", "Ring joint 2", "Ring top",
    "Pinky root", "Pinky joint 1", "Pinky joint 2", "Pinky top"
  };

  SerializedProperty rotationProp, isLeftProp, handProp, nodesProp, collliderProp, confidenceProp;
  AnimBool showNodes;

  void OnEnable() {
    rotationProp = serializedObject.FindProperty("initialRotation");
    isLeftProp = serializedObject.FindProperty("IsLeft");
    handProp = serializedObject.FindProperty("Hand");
    nodesProp = serializedObject.FindProperty("Nodes");
    collliderProp = serializedObject.FindProperty("colliderType");
    confidenceProp = serializedObject.FindProperty("showConficenceAsAlpha");
    showNodes = new AnimBool(true);
    showNodes.valueChanged.AddListener(Repaint);
  }

  public override void OnInspectorGUI() {
    serializedObject.Update();

    GUI.enabled = false;
    SerializedProperty prop = serializedObject.FindProperty("m_Script");
    EditorGUILayout.PropertyField(prop, true, new GUILayoutOption[0]);
    GUI.enabled = true;

    bool isPlaying = Application.isPlaying;
#if UNITY_2018_2_OR_NEWER
    if (!serializedObject.isEditingMultipleObjects)
      isPlaying = Application.IsPlaying(target);
#endif
    GUI.enabled = !isPlaying;

    EditorGUILayout.PropertyField(rotationProp);
    EditorGUILayout.HelpBox("Initial Rotation should make sure palm faces forward, fingers points up in global axis.",
                            MessageType.Info);
    EditorGUILayout.HelpBox("Global rotation of Transform component is locked to use value of Initial Rotation",
                            MessageType.None);
    EditorGUILayout.PropertyField(isLeftProp);
    EditorGUILayout.PropertyField(collliderProp);
    EditorGUILayout.PropertyField(confidenceProp);
    EditorGUILayout.PropertyField(handProp);
    if (nodesProp.hasMultipleDifferentValues)
      EditorGUILayout.LabelField("Nodes", "-");
    else {
      showNodes.target = EditorGUILayout.Foldout(showNodes.target, "Nodes");
      if (EditorGUILayout.BeginFadeGroup(showNodes.faded)) {
        EditorGUI.indentLevel++;
        for (int i = 0; i < 21; i++) {
          var element = nodesProp.GetArrayElementAtIndex(i);
          element.objectReferenceValue =
            EditorGUILayout.ObjectField(names[i], element.objectReferenceValue, typeof(Transform), true);
        }
        EditorGUI.indentLevel--;
      }
      EditorGUILayout.EndFadeGroup();
    }
    serializedObject.ApplyModifiedProperties();
    GUI.enabled = true;

    if (isPlaying)
      return;

    foreach (ModelRenderer target in targets) {
      TransformUtils.SetInspectorRotation(target.transform, target.initialRotation);
      target.initialRotation = TransformUtils.GetInspectorRotation(target.transform);
    }

    if (serializedObject.isEditingMultipleObjects)
      return;

    GUILayout.Space(5);
    if (GUILayout.Button("Auto detect properties"))
      AutoDetect(target as ModelRenderer);
    EditorGUILayout.HelpBox("Please manually check if auto detect set fields correctly. Please refer to help page for details.",
                            MessageType.Info);
  }

  private void AutoDetect(ModelRenderer target) {
    var skinnedMesh = target.transform.GetComponentInChildren<SkinnedMeshRenderer>();
    if (skinnedMesh == null) {
      Debug.LogError("Cannot find SkinnedMeshRenderer in " + target.name);
      return;
    }
    target.Hand = skinnedMesh.gameObject;
    // set finger bones
    SetBone(target, "wrist", 0, 1);
    SetBone(target, "thumb", 1, 4);
    SetBone(target, "index", 5, 4);
    SetBone(target, "middle", 9, 4);
    SetBone(target, "ring", 13, 4);
    SetBone(target, "pinky", 17, 4);

    // check left or right
    int leftCount = CountName(target, "l_", "_l", "_left", "left_");
    int rightCount = CountName(target, "r_", "_r", "_right", "right_");
    if (leftCount == rightCount)
      Debug.LogErrorFormat("Cannot determine left/right for {0}, use current: {1}",
                           target.name, target.IsLeft ? "left" : "right");
    else
      target.IsLeft = leftCount > rightCount;

    // set initial rotation
    var upDir = target.Nodes[9].position - target.Nodes[0].position;
    var indexDir = target.Nodes[5].position - target.Nodes[0].position;
    var palmDir = target.IsLeft ? Vector3.Cross(indexDir, upDir) : Vector3.Cross(upDir, indexDir);
    var rotation = Quaternion.FromToRotation(upDir, Vector3.up);
    var frontDir = rotation * palmDir;
    var angle = Vector3.Angle(frontDir, Vector3.forward);
    var sign = Mathf.Sign(Vector3.Dot(Vector3.up, Vector3.Cross(frontDir, Vector3.forward)));
    rotation = Quaternion.AngleAxis(sign * angle, Vector3.up) * rotation * target.transform.rotation;

    var eulerAngles = rotation.eulerAngles;
    eulerAngles.x = ParseAngle(eulerAngles.x);
    eulerAngles.y = ParseAngle(eulerAngles.y);
    eulerAngles.z = ParseAngle(eulerAngles.z);
    TransformUtils.SetInspectorRotation(target.transform, eulerAngles);
    target.initialRotation = TransformUtils.GetInspectorRotation(target.transform);
  }

  private void SetBone(ModelRenderer target, string name, int boneIndex, int count) {
    var bones = target.GetComponentsInChildren<Transform>()
                .Where(t => t.name.ToLower().Contains(name) && t.GetComponent<SkinnedMeshRenderer>() == null)
                .GroupBy(GetDepth, t => t, (k, v) => v.OrderByDescending(t => t.childCount).ThenBy(t => t.GetSiblingIndex()).First())
                .OrderBy(GetDepth)
                .ToList();
    if (bones.Count < count) {
      Debug.LogErrorFormat("Requires at least {1} bones for {0} finger, found only {2}", name, count, bones.Count);
      return;
    }
    for (int i = bones.Count - count; i < bones.Count; i++, boneIndex++)
      target.Nodes[boneIndex] = bones[i];
  }

  private int GetDepth(Transform t) {
    int d = 0;
    while (t.parent != null) {
      d++;
      t = t.parent;
    }
    return d;
  }

  private int CountName(ModelRenderer target, params string[] names) {
    int maxCount = 0;
    foreach (var name in names) {
      int count = target.Hand.name.ToLower().Contains(name) ? 1 : 0;
      foreach (var node in target.Nodes)
        count += node.name.ToLower().Contains(name) ? 1 : 0;
      if (count > maxCount)
        maxCount = count;
    }
    return maxCount;
  }

  private float ParseAngle(float angle) {
    while (angle < 0)
      angle += 360;
    angle = (int)(angle / 90 + 0.5) * 90;
    if (angle > 180)
      angle -= 360;
    return angle;
  }
}
}
