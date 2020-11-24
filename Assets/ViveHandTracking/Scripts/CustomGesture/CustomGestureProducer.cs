using System;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking {

[Flags]
public enum FingerStateFlag { Close = 1, Relax = 2, Open = 4 }
[Flags]
public enum ThumbStateFlag { Close = 1, Open = 2 }

public enum NodeDistanceType { Near = 0, Far = 1 }

[Serializable]
public class NodeDistanceCondition {
  public int node1;
  public int node2;
  public NodeDistanceType distance;
}

[Serializable]
public class CustomGestureCondition {
  [EnumFlags]
  public ThumbStateFlag Thumb = (ThumbStateFlag)(-1);
  [EnumFlags]
  public FingerStateFlag Index = (FingerStateFlag)(-1);
  [EnumFlags]
  public FingerStateFlag Middle = (FingerStateFlag)(-1);
  [EnumFlags]
  public FingerStateFlag Ring = (FingerStateFlag)(-1);
  [EnumFlags]
  public FingerStateFlag Pinky = (FingerStateFlag)(-1);

  public List<NodeDistanceCondition> FingerTipDistance;

  public bool CheckHandMatch(GestureResult hand, HandState state) {
    if (hand == null || state == null)
      return false;

    if (!Thumb.IsMatch(state.thumb))
      return false;
    if (!Index.IsMatch(state.index))
      return false;
    if (!Middle.IsMatch(state.middle))
      return false;
    if (!Ring.IsMatch(state.ring))
      return false;
    if (!Pinky.IsMatch(state.pinky))
      return false;

    foreach (var condition in FingerTipDistance) {
      var distance = Vector3.Distance(hand.points[condition.node1], hand.points[condition.node2]);
      if (!condition.distance.IsMatch(distance))
        return false;
    }

    return true;
  }
}

public interface ICustomGestureProducer {
  void CheckGesture();
}

public abstract class BaseSingleHandGestureProducer: ScriptableObject, ICustomGestureProducer {
  public bool IsLeftMatch {
    get;
    protected set;
  }

  public bool IsRightMatch {
    get;
    protected set;
  }

  public abstract void CheckGesture();
}

public abstract class BaseDualHandGestureProducer: ScriptableObject, ICustomGestureProducer {
  public bool IsMatch {
    get;
    protected set;
  }

  public abstract void CheckGesture();
}

public static class CustomGestureHelper {
  public static bool IsMatch(this FingerStateFlag condition, FingerState state) {
    var stateCond = (FingerStateFlag)(1 << (int)state);
    return (stateCond & condition) == stateCond;
  }

  public static bool IsMatch(this ThumbStateFlag condition, ThumbState state) {
    var stateCond = (ThumbStateFlag)(1 << (int)state);
    return (stateCond & condition) == stateCond;
  }

  public static bool IsMatch(this NodeDistanceType type, float distance, bool SingleHand = true) {
    if (type == NodeDistanceType.Near)
      return distance < (SingleHand ? 0.025f : 0.1f);
    return distance > (SingleHand ? 0.05f : 0.2f);
  }
}

// Note: attach to NodeDistanceCondition or List<NodeDistanceCondition> if it's used for dual hand
public class DualHandAttribute : PropertyAttribute {
  public DualHandAttribute() {}
}
}
