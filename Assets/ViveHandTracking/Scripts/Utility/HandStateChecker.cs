using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Events;

namespace ViveHandTracking {

[Flags]
public enum HandFlag {
  NoHand = 1,
  Unknown = 2, Point = 4, Fist = 8, OK = 16, Like = 32, Five = 64, Victory = 128,
  Pinch = 1 << 13,
  // single hand custom gesture
  CustomGesture1 = 1 << 16, CustomGesture2 = 1 << 17, CustomGesture3 = 1 << 18, CustomGesture4 = 1 << 19,
  CustomGesture5 = 1 << 20, CustomGesture6 = 1 << 21, CustomGesture7 = 1 << 22, CustomGesture8 = 1 << 23,
  CustomGesture9 = 1 << 24, CustomGesture10 = 1 << 25,
  // dual hand custom gesture
  DualGesture1 = 1 << 26, DualGesture2 = 1 << 27, DualGesture3 = 1 << 28,
  DualGesture4 = 1 << 29, DualGesture5 = 1 << 30,
}

[Serializable]
class HandResetCondition {
  [Tooltip("Reset immediately if left hand is missing")]
  public bool LeftHandMissing = true;
  [Tooltip("Reset immediately if right hand is missing")]
  public bool RightHandMissing = true;
}

[Serializable]
class HandStateCondition {
  [Tooltip("Left hand gesture to match. Select Nothing to never enter this condition.")]
  public HandFlag Left = (HandFlag)(-1);
  [Tooltip("Right hand gesture to match. Select Nothing to never enter this condition.")]
  public HandFlag Right = (HandFlag)(-1);
  [Tooltip("How many continous frames of matched hand state before entering this state.")]
  [Range(0, 20)]
  public int MinMatchFrames = 4;
  [Tooltip("How many continous frames of non-matched hand state before leaving this state.")]
  [Range(0, 30)]
  public int MaxMissingFrames = 20;
}

[Serializable]
class HandStateChangeEvent : UnityEvent<int> {}

[HelpURL("https://hub.vive.com/storage/tracking/unity/advanced.html#using-gesture-as-events")]
class HandStateChecker : MonoBehaviour {
  private const HandFlag HandFlagNone = (HandFlag)0;

  [Tooltip("Conditions for reseting to state 0")]
  public HandResetCondition ResetCondition = null;
  [Tooltip("Conditions for state 1")]
  public HandStateCondition PrepareCondition = null;
  [Tooltip("Conditions for state 2")]
  public HandStateCondition TriggerCondition = null;
  [Tooltip("Allow enter state 2 from state 0")]
  public bool CanSkipPrepare = false;

  public HandStateChangeEvent OnStateChanged = null;

  // 0 - None, 1 - Prepare, 2 - Trigger
  public int state {
    get;
    private set;
  }
  private int PrepareMatchCounter = 0;
  private int TriggerMatchCounter = 0;
  private int MissingCounter = 0;

  void Update () {
    HandFlag LeftFlag = GetFlag(GestureProvider.LeftHand) | GetCustomFlag(true);
    HandFlag RightFlag = GetFlag(GestureProvider.RightHand) | GetCustomFlag(false);

    if ((ResetCondition.LeftHandMissing && LeftFlag == HandFlag.NoHand) ||
        (ResetCondition.RightHandMissing && RightFlag == HandFlag.NoHand)) {
      SetState(0);
      MissingCounter = 0;
      PrepareMatchCounter = PrepareCondition.MinMatchFrames;
    } else if (IsFlagMatch(LeftFlag, RightFlag, PrepareCondition)) {
      if (PrepareMatchCounter > 0)
        PrepareMatchCounter--;
      else {
        SetState(1);
        MissingCounter = PrepareCondition.MaxMissingFrames;
        PrepareMatchCounter = 0;
        TriggerMatchCounter = TriggerCondition.MinMatchFrames;
      }
    } else if ((state != 0 || CanSkipPrepare) && IsFlagMatch(LeftFlag, RightFlag, TriggerCondition)) {
      if (TriggerMatchCounter > 0)
        TriggerMatchCounter--;
      else {
        SetState(2);
        MissingCounter = TriggerCondition.MaxMissingFrames;
        TriggerMatchCounter = 0;
        PrepareMatchCounter = PrepareCondition.MinMatchFrames;
      }
    } else if (MissingCounter > 0)
      MissingCounter--;
    else {
      SetState(0);
      MissingCounter = 0;
      PrepareMatchCounter = PrepareCondition.MinMatchFrames;
    }
  }

  HandFlag GetFlag(GestureResult hand) {
    var flag = HandFlag.NoHand;
    if (hand != null) {
      flag = (HandFlag)(2 << (int)hand.gesture);
      if (hand.pinch.isPinching)
        flag |= HandFlag.Pinch;
    }
    return flag;
  }

  HandFlag GetCustomFlag(bool isLeft) {
    var flag = HandFlagNone;
    if (CustomGestureProvider.Current == null)
      return flag;

    var current = HandFlag.CustomGesture1;
    int size = Math.Min(CustomGestureProvider.MaxSingleHandCustomStates,
                        CustomGestureProvider.Current.SingleHandCustomGestures.Count);
    for (int i = 0; i < size; i++) {
      var producer = CustomGestureProvider.Current.SingleHandCustomGestures[i];
      if (isLeft ? producer.IsLeftMatch : producer.IsRightMatch)
        flag |= current;
      current = (HandFlag)(((int)current) << 1);
    }

    current = HandFlag.DualGesture1;
    size = Math.Min(CustomGestureProvider.MaxDualHandCustomStates,
                    CustomGestureProvider.Current.DualHandCustomGestures.Count);
    for (int i = 0; i < size; i++) {
      if (CustomGestureProvider.Current.DualHandCustomGestures[i].IsMatch)
        flag |= current;
      current = (HandFlag)(((int)current) << 1);
    }
    return flag;
  }

  bool IsFlagMatch(HandFlag left, HandFlag right, HandStateCondition condition) {
    return ((left & condition.Left) != HandFlagNone) && ((right & condition.Right) != HandFlagNone);
  }

  void SetState(int newState) {
    if (state != newState && OnStateChanged != null)
      OnStateChanged.Invoke((int)newState);
    state = newState;
  }
}

}
