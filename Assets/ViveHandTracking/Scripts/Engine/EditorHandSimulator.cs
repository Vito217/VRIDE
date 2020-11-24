using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.AnimatedValues;
#endif

namespace ViveHandTracking {

#if UNITY_EDITOR

enum HandSimulationPreset {
  Invisible, Unknown,
  Point, Fist, OK, Like, Five, Victory,
  PinchOpen, Pinch,
}

enum FingerSimulationState { Close, Open, Relax, Pinch }

[Serializable]
struct HandSimulationState {
  public FingerSimulationState thumb;
  public FingerSimulationState index;
  public FingerSimulationState middle;
  public FingerSimulationState ring;
  public FingerSimulationState pinky;
  public HandSimulationPreset preset;
  public Transform anchor;
  [Range(0f, 1f)]
  public float confidence;
}

[HelpURL("https://hub.vive.com/storage/tracking/unity/engine.html#using-editor-engine")]
class EditorHandSimulator : MonoBehaviour {
  [Tooltip("Hand pose is relative to head")]
  public bool FollowHead = true;
  public HandSimulationState LeftState;
  public HandSimulationState RightState;
  [SerializeField]
  private Transform HandAnchor = null;
  internal bool hasSkeleton = true;

  public GestureResultRaw LeftHand {
    get {
      return LeftState.preset == HandSimulationPreset.Invisible ? null : leftHand;
    }
  }
  private GestureResultRaw leftHand;

  public GestureResultRaw RightHand {
    get {
      return RightState.preset == HandSimulationPreset.Invisible ? null : rightHand;
    }
  }
  private GestureResultRaw rightHand;

  void Awake() {
    transform.position = GestureProvider.Current.transform.position;
    transform.rotation = GestureProvider.Current.transform.rotation;
    transform.localScale = GestureProvider.Current.transform.lossyScale;

    var go = new GameObject("Hand Anchor");
    HandAnchor = go.transform;
    go.transform.SetParent(transform, false);
    go.transform.localPosition = Vector3.zero;
    go.transform.localRotation = Quaternion.identity;
    go.transform.localScale = Vector3.one;

    LeftState = CreateState(true);
    RightState = CreateState(false);
    leftHand = CreateHand(true);
    rightHand = CreateHand(false);
  }

  void Update() {
    if (FollowHead) {
      transform.position = GestureProvider.Current.transform.position;
      transform.rotation = GestureProvider.Current.transform.rotation;
    }
    SetHand(LeftState, leftHand);
    SetHand(RightState, rightHand);
  }

  GestureResultRaw CreateHand(bool left) {
    var hand = new GestureResultRaw();
    hand.isLeft = left;
    hand.points = new Vector3[21];
    return hand;
  }

  HandSimulationState CreateState(bool left) {
    var state = new HandSimulationState();
    state.preset = HandSimulationPreset.Five;
    state.thumb = FingerSimulationState.Open;
    state.index = FingerSimulationState.Open;
    state.middle = FingerSimulationState.Open;
    state.ring = FingerSimulationState.Open;
    state.pinky = FingerSimulationState.Open;
    var go = new GameObject(left ? "Left Anchor" : "Right Anchor");
    state.anchor = go.transform;
    go.transform.SetParent(HandAnchor, false);
    go.transform.localPosition = new Vector3(left ? -0.12f : 0.12f, 0.03f, 0.5f);
    go.transform.localRotation = Quaternion.identity;
    go.transform.localScale = Vector3.one;
    state.confidence = 1;
    return state;
  }

  void SetHand(HandSimulationState state, GestureResultRaw hand) {
    if (state.preset == HandSimulationPreset.Invisible)
      return;
    hand.pinchLevel = 0;
    switch (state.preset) {
    case HandSimulationPreset.Point :
      hand.gesture = GestureType.Point;
      break;
    case HandSimulationPreset.Fist:
      hand.gesture = GestureType.Fist;
      break;
    case HandSimulationPreset.OK:
      hand.gesture = GestureType.OK;
      hand.pinchLevel = 1;
      break;
    case HandSimulationPreset.Like:
      hand.gesture = GestureType.Like;
      break;
    case HandSimulationPreset.Five:
      hand.gesture = GestureType.Five;
      break;
    case HandSimulationPreset.Victory:
      hand.gesture = GestureType.Victory;
      break;
    case HandSimulationPreset.Pinch:
      hand.gesture = GestureType.Unknown;
      hand.pinchLevel = 1;
      break;
    default:
      hand.gesture = GestureType.Unknown;
      break;
    }
    hand.points[0] = state.anchor.position;
    if (hasSkeleton) {
      SetPoints(hand, 1, ThumbMap[state.thumb], state.anchor);
      SetPoints(hand, 5, IndexMap[state.index], state.anchor);
      SetPoints(hand, 9, MiddleMap[state.middle], state.anchor);
      SetPoints(hand, 13, RingMap[state.ring], state.anchor);
      SetPoints(hand, 17, PinkyMap[state.pinky], state.anchor);
    }
    hand.confidence = state.confidence;
  }

  void SetPoints(GestureResultRaw hand, int startIndex, Vector3[] points, Transform parent) {
    var pose = points[0];
    if (hand.isLeft)
      pose.x = -pose.x;
    hand.points[startIndex] = parent.rotation * pose + parent.position;

    pose = points[1];
    if (hand.isLeft)
      pose.x = -pose.x;
    hand.points[startIndex + 1] = parent.rotation * pose + parent.position;

    pose = points[2];
    if (hand.isLeft)
      pose.x = -pose.x;
    hand.points[startIndex + 2] = parent.rotation * pose + parent.position;

    pose = points[3];
    if (hand.isLeft)
      pose.x = -pose.x;
    hand.points[startIndex + 3] = parent.rotation * pose + parent.position;
  }

  private static Dictionary<FingerSimulationState, Vector3[]> ThumbMap;
  private static Dictionary<FingerSimulationState, Vector3[]> IndexMap;
  private static Dictionary<FingerSimulationState, Vector3[]> MiddleMap;
  private static Dictionary<FingerSimulationState, Vector3[]> RingMap;
  private static Dictionary<FingerSimulationState, Vector3[]> PinkyMap;

  static EditorHandSimulator() {
    ThumbMap = new Dictionary<FingerSimulationState, Vector3[]>();
    ThumbMap[FingerSimulationState.Close] = new Vector3[] {
      new Vector3(-0.02561346f, 0.02549528f, 0f), new Vector3(-0.03271355f, 0.05344393f, 0.02630469f),
      new Vector3(-0.03909954f, 0.07858165f, 0.0499638f), new Vector3(-0.01853538f, 0.09150755f, 0.0621646f),
    };
    ThumbMap[FingerSimulationState.Open] = new Vector3[] {
      new Vector3(-0.02561346f, 0.02549528f, 0f), new Vector3(-0.05091827f, 0.055213f, 0f),
      new Vector3(-0.07367802f, 0.08194186f, 0f), new Vector3(-0.08944285f, 0.1004559f, 0f),
    };
    ThumbMap[FingerSimulationState.Pinch] = new Vector3[] {
      new Vector3(-0.02561346f, 0.02549528f, 0f), new Vector3(-0.03026959f, 0.0487665f, 0.03098783f),
      new Vector3(-0.03445743f, 0.06969722f, 0.05885908f), new Vector3(-0.03735819f, 0.08419512f, 0.07816443f),
    };
    ThumbMap[FingerSimulationState.Relax] = ThumbMap[FingerSimulationState.Pinch];

    IndexMap = new Dictionary<FingerSimulationState, Vector3[]>();
    IndexMap[FingerSimulationState.Close] = new Vector3[] {
      new Vector3(-0.0234468f, 0.08993375f, 0f), new Vector3(-0.03007099f, 0.09965698f, 0.03628755f),
      new Vector3(-0.03120193f, 0.07783888f, 0.04877388f), new Vector3(-0.0279232f, 0.0624964f, 0.03363425f),
    };
    IndexMap[FingerSimulationState.Open] = new Vector3[] {
      new Vector3(-0.0234468f, 0.08993375f, 0f), new Vector3(-0.03007099f, 0.1275014f, 0f),
      new Vector3(-0.03444064f, 0.1522829f, 0f), new Vector3(-0.0382266f, 0.1737542f, 0f),
    };
    IndexMap[FingerSimulationState.Relax] = new Vector3[] {
      new Vector3(-0.0234468f, 0.08993375f, 0f), new Vector3(-0.03007099f, 0.1224683f, 0.01878384f),
      new Vector3(-0.03385521f, 0.1347634f, 0.04041079f), new Vector3(-0.0357482f, 0.13462f, 0.06213051f),
    };
    IndexMap[FingerSimulationState.Pinch] = new Vector3[] {
      new Vector3(-0.0234468f, 0.08993375f, 0f), new Vector3(-0.03007099f, 0.1058105f, 0.03404784f),
      new Vector3(-0.03377666f, 0.1026068f, 0.05873024f), new Vector3(-0.03610754f, 0.09262248f, 0.07797164f),
    };

    MiddleMap = new Dictionary<FingerSimulationState, Vector3[]>();
    MiddleMap[FingerSimulationState.Close] = new Vector3[] {
      new Vector3(0f, 0.09059203f, 0f), new Vector3(0.002243221f, 0.1016704f, 0.04134488f),
      new Vector3(0.002620131f, 0.07757138f, 0.05524743f), new Vector3(0.00155279f, 0.06092726f, 0.03862306f),
    };
    MiddleMap[FingerSimulationState.Open] = new Vector3[] {
      new Vector3(0f, 0.09059203f, 0f), new Vector3(0.002243221f, 0.1333954f, 0f),
      new Vector3(0.003699422f, 0.1611814f, 0f), new Vector3(0.004931867f, 0.1846977f, 0f),
    };
    MiddleMap[FingerSimulationState.Relax] = new Vector3[] {
      new Vector3(0f, 0.09059203f, 0f), new Vector3(0.002243221f, 0.1276608f, 0.02140167f),
      new Vector3(0.003504336f, 0.1415443f, 0.04548159f), new Vector3(0.004120559f, 0.1415303f, 0.06902212f),
    };
    MiddleMap[FingerSimulationState.Pinch] = MiddleMap[FingerSimulationState.Relax];

    RingMap = new Dictionary<FingerSimulationState, Vector3[]>();
    RingMap[FingerSimulationState.Close] = new Vector3[] {
      new Vector3(0.01830575f, 0.08586f, 0f), new Vector3(0.02556321f, 0.09651273f, 0.03975654f),
      new Vector3(0.02667755f, 0.07501508f, 0.05205947f), new Vector3(0.02360314f, 0.06062887f, 0.03786346f),
    };
    RingMap[FingerSimulationState.Open] = new Vector3[] {
      new Vector3(0.01830575f, 0.08586f, 0f), new Vector3(0.02556321f, 0.127019f, 0f),
      new Vector3(0.02986869f, 0.1514365f, 0f), new Vector3(0.03341869f, 0.1715696f, 0f),
    };
    RingMap[FingerSimulationState.Relax] = new Vector3[] {
      new Vector3(0.01830575f, 0.08586f, 0f), new Vector3(0.02556321f, 0.1215047f, 0.02057949f),
      new Vector3(0.02929184f, 0.1336193f, 0.0418888f), new Vector3(0.03106686f, 0.1334848f, 0.06225482f),
    };
    RingMap[FingerSimulationState.Pinch] = RingMap[FingerSimulationState.Relax];

    PinkyMap = new Dictionary<FingerSimulationState, Vector3[]>();
    PinkyMap[FingerSimulationState.Close] = new Vector3[] {
      new Vector3(0.03541002f, 0.07745709f, 0f), new Vector3(0.04617861f, 0.08511462f, 0.02857831f),
      new Vector3(0.04784632f, 0.06872287f, 0.03771409f), new Vector3(0.04305059f, 0.05749299f, 0.02708218f),
    };
    PinkyMap[FingerSimulationState.Open] = new Vector3[] {
      new Vector3(0.03541002f, 0.07745709f, 0f), new Vector3(0.04617861f, 0.1070435f, 0f),
      new Vector3(0.05262217f, 0.124747f, 0f), new Vector3(0.0581598f, 0.1399616f, 0f),
    };
    PinkyMap[FingerSimulationState.Relax] = new Vector3[] {
      new Vector3(0.03541002f, 0.07745709f, 0f), new Vector3(0.04617861f, 0.1030797f, 0.01479322f),
      new Vector3(0.05175889f, 0.1116474f, 0.03061688f), new Vector3(0.0545277f, 0.1112246f, 0.04656374f),
    };
    PinkyMap[FingerSimulationState.Pinch] = PinkyMap[FingerSimulationState.Relax];
  }
}

[CustomEditor(typeof(EditorHandSimulator))]
class EditorHandSimulatorEditor : Editor {
  private static readonly string[] ThumbStateNames = new string[] { "Close", "Open", "Pinch" };
  private static readonly FingerSimulationState[] ThumbStates = new FingerSimulationState[] {
    FingerSimulationState.Close, FingerSimulationState.Open, FingerSimulationState.Pinch
  };
  private static readonly string[] IndexStateNames = new string[] { "Close", "Open", "Relax", "Pinch" };
  private static readonly string[] FingerStateNames = new string[] { "Close", "Open", "Relax" };

  SerializedProperty leftStateProp, rightStateProp, followHeadProp, handAnchorProp;
  bool showLeft = true, showRight = true;

  void OnEnable() {
    leftStateProp = serializedObject.FindProperty("LeftState");
    rightStateProp = serializedObject.FindProperty("RightState");
    followHeadProp = serializedObject.FindProperty("FollowHead");
    handAnchorProp = serializedObject.FindProperty("HandAnchor");
  }

  public override void OnInspectorGUI() {
    EditorGUILayout.PropertyField(followHeadProp, new GUIContent("Follow Head"));
    var anchor = handAnchorProp.objectReferenceValue as Transform;
    if (anchor != null) {
      var label = new GUIContent("Hand Base Position");
      label.tooltip = "Use this to change both hands position together";
      anchor.localPosition = EditorGUILayout.Vector3Field(label, anchor.localPosition);
    }
    GUILayout.Space(15);
    DrawState(leftStateProp, ref showLeft, "Left Hand");
    GUILayout.Space(25);
    DrawState(rightStateProp, ref showRight, "Right Hand");
    serializedObject.ApplyModifiedProperties();
  }

  void DrawState(SerializedProperty prop, ref bool expand, string name) {
    bool hasSkeleton = (target as EditorHandSimulator).hasSkeleton;

    var anchor = prop.FindPropertyRelative("anchor").objectReferenceValue as Transform;
    if (anchor != null) {
      anchor.localPosition = EditorGUILayout.Vector3Field(name + " Position", anchor.localPosition);
      if (hasSkeleton) {
        var angles = TransformUtils.GetInspectorRotation(anchor);
        angles = EditorGUILayout.Vector3Field(name + " Rotation", angles);
        TransformUtils.SetInspectorRotation(anchor, angles);
      }
      GUILayout.Space(5);
    }
    EditorGUILayout.PropertyField(prop.FindPropertyRelative("confidence"), new GUIContent(name + " Confidence"));

    expand = EditorGUILayout.Foldout(expand, name + " State", true);
    if (EditorGUILayout.BeginFadeGroup(expand ? 1 : 0)) {
      EditorGUI.indentLevel++;
      EditorGUILayout.BeginHorizontal();
      EditorGUILayout.PrefixLabel("Preset");
      var presetProp = prop.FindPropertyRelative("preset");
      int newValue = GUILayout.SelectionGrid(presetProp.intValue, presetProp.enumNames, 3);
      bool changed = presetProp.intValue != newValue;
      presetProp.intValue = newValue;
      EditorGUILayout.EndHorizontal();
      if (changed)
        ApplyPreset(prop, (HandSimulationPreset)newValue);

      if (hasSkeleton) {
        GUILayout.Space(5);
        GUI.enabled = presetProp.intValue > 0;
        changed = DrawFinger(prop.FindPropertyRelative("thumb"), ThumbStateNames, ThumbStates);
        changed = DrawFinger(prop.FindPropertyRelative("index"), IndexStateNames) || changed;
        changed = DrawFinger(prop.FindPropertyRelative("middle"), FingerStateNames) || changed;
        changed = DrawFinger(prop.FindPropertyRelative("ring"), FingerStateNames) || changed;
        changed = DrawFinger(prop.FindPropertyRelative("pinky"), FingerStateNames) || changed;
        GUI.enabled = true;
        if (changed)
          CalculatePreset(prop, presetProp);
      }
      EditorGUI.indentLevel--;
    }
    EditorGUILayout.EndFadeGroup();
  }

  bool DrawFinger(SerializedProperty prop, string[] stateNames, FingerSimulationState[] states = null) {
    EditorGUILayout.BeginHorizontal();
    var name = char.ToUpper(prop.name[0]) + prop.name.Substring(1) + " State";
    EditorGUILayout.PrefixLabel(name);
    int value = prop.intValue;
    if (states != null)
      value = Array.IndexOf(states, (FingerSimulationState)(value));
    value = GUILayout.Toolbar(value, stateNames);
    if (states != null)
      value = (int)states[value];
    EditorGUILayout.EndHorizontal();
    if (prop.intValue != value) {
      prop.intValue = value;
      return true;
    }
    return false;
  }

  void ApplyPreset(SerializedProperty prop, HandSimulationPreset preset) {
    switch (preset) {
    case HandSimulationPreset.Unknown:
      SetFinger(prop, "thumb", FingerSimulationState.Open);
      SetFinger(prop, "index", FingerSimulationState.Relax);
      SetFinger(prop, "middle", FingerSimulationState.Relax);
      SetFinger(prop, "ring", FingerSimulationState.Relax);
      SetFinger(prop, "pinky", FingerSimulationState.Relax);
      break;
    case HandSimulationPreset.Point:
      SetFinger(prop, "thumb", FingerSimulationState.Close);
      SetFinger(prop, "index", FingerSimulationState.Open);
      SetFinger(prop, "middle", FingerSimulationState.Close);
      SetFinger(prop, "ring", FingerSimulationState.Close);
      SetFinger(prop, "pinky", FingerSimulationState.Close);
      break;
    case HandSimulationPreset.Fist:
      SetFinger(prop, "thumb", FingerSimulationState.Close);
      SetFinger(prop, "index", FingerSimulationState.Close);
      SetFinger(prop, "middle", FingerSimulationState.Close);
      SetFinger(prop, "ring", FingerSimulationState.Close);
      SetFinger(prop, "pinky", FingerSimulationState.Close);
      break;
    case HandSimulationPreset.OK:
      SetFinger(prop, "thumb", FingerSimulationState.Pinch);
      SetFinger(prop, "index", FingerSimulationState.Pinch);
      SetFinger(prop, "middle", FingerSimulationState.Open);
      SetFinger(prop, "ring", FingerSimulationState.Open);
      SetFinger(prop, "pinky", FingerSimulationState.Open);
      break;
    case HandSimulationPreset.Like:
      SetFinger(prop, "thumb", FingerSimulationState.Open);
      SetFinger(prop, "index", FingerSimulationState.Close);
      SetFinger(prop, "middle", FingerSimulationState.Close);
      SetFinger(prop, "ring", FingerSimulationState.Close);
      SetFinger(prop, "pinky", FingerSimulationState.Close);
      break;
    case HandSimulationPreset.Five:
      SetFinger(prop, "thumb", FingerSimulationState.Open);
      SetFinger(prop, "index", FingerSimulationState.Open);
      SetFinger(prop, "middle", FingerSimulationState.Open);
      SetFinger(prop, "ring", FingerSimulationState.Open);
      SetFinger(prop, "pinky", FingerSimulationState.Open);
      break;
    case HandSimulationPreset.Victory:
      SetFinger(prop, "thumb", FingerSimulationState.Close);
      SetFinger(prop, "index", FingerSimulationState.Open);
      SetFinger(prop, "middle", FingerSimulationState.Open);
      SetFinger(prop, "ring", FingerSimulationState.Close);
      SetFinger(prop, "pinky", FingerSimulationState.Close);
      break;
    case HandSimulationPreset.PinchOpen:
      SetFinger(prop, "thumb", FingerSimulationState.Pinch);
      SetFinger(prop, "index", FingerSimulationState.Relax);
      break;
    case HandSimulationPreset.Pinch:
      SetFinger(prop, "thumb", FingerSimulationState.Pinch);
      SetFinger(prop, "index", FingerSimulationState.Pinch);
      var middle = (FingerSimulationState)prop.FindPropertyRelative("middle").intValue;
      var ring = (FingerSimulationState)prop.FindPropertyRelative("ring").intValue;
      var pinky = (FingerSimulationState)prop.FindPropertyRelative("pinky").intValue;
      if (middle == FingerSimulationState.Open && middle == ring && middle == pinky) {
        SetFinger(prop, "middle", FingerSimulationState.Close);
        SetFinger(prop, "ring", FingerSimulationState.Close);
        SetFinger(prop, "pinky", FingerSimulationState.Close);
      }
      break;
    }
  }

  void SetFinger(SerializedProperty prop, string name, FingerSimulationState state) {
    prop.FindPropertyRelative(name).intValue = (int)state;
  }

  void CalculatePreset(SerializedProperty prop, SerializedProperty presetProp) {
    var thumb = (FingerSimulationState)prop.FindPropertyRelative("thumb").intValue;
    var index = (FingerSimulationState)prop.FindPropertyRelative("index").intValue;
    var middle = (FingerSimulationState)prop.FindPropertyRelative("middle").intValue;
    var ring = (FingerSimulationState)prop.FindPropertyRelative("ring").intValue;
    var pinky = (FingerSimulationState)prop.FindPropertyRelative("pinky").intValue;

    var preset = HandSimulationPreset.Unknown;
    if (thumb == FingerSimulationState.Pinch) {
      if (index == FingerSimulationState.Relax)
        preset = HandSimulationPreset.PinchOpen;
      else if (index == FingerSimulationState.Pinch) {
        if (middle == FingerSimulationState.Open && middle == ring && middle == pinky)
          preset = HandSimulationPreset.OK;
        else
          preset = HandSimulationPreset.Pinch;
      }
    } else if (thumb == FingerSimulationState.Close && middle == index && thumb == ring && thumb == pinky) {
      if (index == FingerSimulationState.Open)
        preset = HandSimulationPreset.Victory;
      else if (index == FingerSimulationState.Close)
        preset = HandSimulationPreset.Fist;
    } else if (thumb == FingerSimulationState.Open && index == middle && index == ring && index == pinky) {
      if (index == FingerSimulationState.Close)
        preset = HandSimulationPreset.Like;
      else if (index == FingerSimulationState.Open)
        preset = HandSimulationPreset.Five;
    }
    if (index == FingerSimulationState.Open && middle == FingerSimulationState.Close && middle == ring && middle == pinky)
      preset = HandSimulationPreset.Point;

    presetProp.intValue = (int)preset;
  }
}

#endif

}
