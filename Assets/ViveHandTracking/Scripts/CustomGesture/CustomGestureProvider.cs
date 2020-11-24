using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking {

public enum FingerState { Close, Relax, Open };
public enum ThumbState { Close, Open };

public class HandState {
  public ThumbState thumb;
  public FingerState index;
  public FingerState middle;
  public FingerState ring;
  public FingerState pinky;
}

[HelpURL("https://hub.vive.com/storage/tracking/unity/advanced.html#customgestureunity")]
public class CustomGestureProvider : MonoBehaviour {
  // max number of single hand custom gestures that can be used in HandStateChcker
  // you can define more custom gestures, but you need to check gesture state manually
  public const int MaxSingleHandCustomStates = 10;
  // max number of dual hand custom gestures that can be used in HandStateChcker
  // you can define more custom gestures, but you need to check gesture state manually
  public const int MaxDualHandCustomStates = 5;

  // Returns the current singleton (or null if no instance exists).
  public static CustomGestureProvider Current {
    get;
    private set;
  }
  public static HandState LeftHandState {
    get;
    private set;
  }
  public static HandState RightHandState {
    get;
    private set;
  }

  public List<BaseSingleHandGestureProducer> SingleHandCustomGestures;
  public List<BaseDualHandGestureProducer> DualHandCustomGestures;

  private HandState leftState, rightState;

  void Awake() {
    if (Current != null) {
      Debug.LogWarning("Only one CustomGestureProvider is allowed in the scene.");
      GameObject.Destroy(this);
      return;
    }
    Current = this;
    leftState = new HandState();
    rightState = new HandState();
    LeftHandState = null;
    RightHandState = null;
  }

  void Update () {
    if (!GestureProvider.HaveSkeleton) {
      Debug.LogError("CustomGestureProvider is only supported in Skeleton mode");
      this.enabled = false;
      LeftHandState = null;
      RightHandState = null;
      return;
    }
    LeftHandState = SetHandState(GestureProvider.LeftHand, leftState);
    RightHandState = SetHandState(GestureProvider.RightHand, rightState);

    foreach (var producer in SingleHandCustomGestures)
      producer.CheckGesture();
    foreach (var producer in DualHandCustomGestures)
      producer.CheckGesture();
  }

  private static HandState SetHandState(GestureResult hand, HandState state) {
    if (hand == null)
      return null;
    state.thumb = GetThumbState(hand.points[2], hand.points[3], hand.points[4]);
    state.index = GetFingerState(hand.points[5], hand.points[6], hand.points[8]);
    state.middle = GetFingerState(hand.points[9], hand.points[10], hand.points[12]);
    state.ring = GetFingerState(hand.points[13], hand.points[14], hand.points[16]);
    state.pinky = GetFingerState(hand.points[17], hand.points[18], hand.points[20]);
    return state;
  }

  private static ThumbState GetThumbState(Vector3 root, Vector3 node1, Vector3 top) {
    var angle = Vector3.Angle(node1 - root, top - node1);
    return angle < 15 ? ThumbState.Open : ThumbState.Close;
  }

  private static FingerState GetFingerState(Vector3 root, Vector3 node1, Vector3 top) {
    var angle = Vector3.Angle(node1 - root, top - node1);
    if (angle < 20)
      return FingerState.Open;
    if (angle > 75)
      return FingerState.Close;
    return FingerState.Relax;
  }
}

}
