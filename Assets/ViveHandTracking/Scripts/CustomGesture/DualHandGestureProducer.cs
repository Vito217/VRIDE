using UnityEngine;
using System.Collections.Generic;

namespace ViveHandTracking {

[CreateAssetMenu(fileName = "CustomGesture", menuName = "ViveHandTracking/Custom Gesture (Dual Hand)", order = 251)]
public class DualHandGestureProducer : BaseDualHandGestureProducer {
  public CustomGestureCondition leftCondition;
  public CustomGestureCondition rightCondition;

  [DualHand]
  public List<NodeDistanceCondition> CrossHandFingerTipDistance;

  public override void CheckGesture() {
    IsMatch = false;

    if (!leftCondition.CheckHandMatch(GestureProvider.LeftHand, CustomGestureProvider.LeftHandState))
      return;
    if (!rightCondition.CheckHandMatch(GestureProvider.RightHand, CustomGestureProvider.RightHandState))
      return;

    foreach (var condition in CrossHandFingerTipDistance) {
      var distance = Vector3.Distance(GestureProvider.LeftHand.points[condition.node1],
                                      GestureProvider.RightHand.points[condition.node2]);
      if (!condition.distance.IsMatch(distance, false))
        return;
    }

    IsMatch = true;
  }
}

}
