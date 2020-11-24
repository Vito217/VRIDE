using UnityEngine;

namespace ViveHandTracking {

[CreateAssetMenu(fileName = "CustomGesture", menuName = "ViveHandTracking/Custom Gesture (Single Hand)", order = 250)]
public class SingleHandGestureProducer : BaseSingleHandGestureProducer {
  public CustomGestureCondition condition;

  public override void CheckGesture() {
    IsLeftMatch = condition.CheckHandMatch(GestureProvider.LeftHand, CustomGestureProvider.LeftHandState);
    IsRightMatch = condition.CheckHandMatch(GestureProvider.RightHand, CustomGestureProvider.RightHandState);
  }
}

}
