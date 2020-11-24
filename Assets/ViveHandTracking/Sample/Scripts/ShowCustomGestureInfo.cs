using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

namespace ViveHandTracking.Sample {

class ShowCustomGestureInfo : MonoBehaviour {
  public Text LeftState = null;
  public Text RightState = null;
  public Text SingleHandState = null;
  public Text DualHandState = null;

  void Update() {
    DisplayText(LeftState, GestureProvider.LeftHand, CustomGestureProvider.LeftHandState);
    DisplayText(RightState, GestureProvider.RightHand, CustomGestureProvider.RightHandState);

    if (CustomGestureProvider.Current == null) {
      SingleHandState.text = "CustomGestureProvider not found";
      DualHandState.text = "";
      return;
    }

    SingleHandState.text = "Single Hand Custom Gesture:" + System.Environment.NewLine;
    foreach (var producer in CustomGestureProvider.Current.SingleHandCustomGestures)
      SingleHandState.text += string.Format("{0}: Left {1} Right {2}", producer.name, IsOn(producer.IsLeftMatch),
                                            IsOn(producer.IsRightMatch)) + System.Environment.NewLine;

    DualHandState.text = "Dual Hand Custom Gesture:" + System.Environment.NewLine;
    foreach (var producer in CustomGestureProvider.Current.DualHandCustomGestures)
      DualHandState.text += string.Format("{0}: {1}", producer.name, IsOn(producer.IsMatch)) + System.Environment.NewLine;
  }

  void DisplayText(Text text, GestureResult hand, HandState state) {
    text.text = text == LeftState ? "Left Hand:" : "Right Hand:";
    if (hand == null) {
      text.text += " Not visible";
      return;
    }
    text.text += System.Environment.NewLine;
    text.text += "Thumb: " + state.thumb + System.Environment.NewLine;
    text.text += "Index: " + state.index + System.Environment.NewLine;
    text.text += "Middle: " + state.middle + System.Environment.NewLine;
    text.text += "Ring: " + state.ring + System.Environment.NewLine;
    text.text += "Pinky: " + state.pinky + System.Environment.NewLine;
  }

  string IsOn(bool match) {
    return match ? "Yes" : "No";
  }
}

}
