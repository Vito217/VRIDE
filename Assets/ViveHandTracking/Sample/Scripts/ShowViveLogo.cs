using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

class ShowViveLogo : MonoBehaviour {
  public GameObject Logo = null;

  void Start() {
    Logo.SetActive(false);
    enabled = false;
  }

  void Update() {
    var leftThumbRoot = GestureProvider.LeftHand.points[1];
    var leftIndexTip = GestureProvider.LeftHand.points[8];
    var rightThumbRoot = GestureProvider.RightHand.points[1];
    var rightIndexTip = GestureProvider.RightHand.points[8];

    transform.position = (leftThumbRoot + leftIndexTip + rightThumbRoot + rightIndexTip) / 4;
    var up = (leftIndexTip + rightIndexTip - leftThumbRoot - rightThumbRoot);
    var right = rightThumbRoot - leftThumbRoot;
    var forward = Vector3.Cross(right, up);
    transform.rotation = Quaternion.LookRotation(forward, up);
    transform.localScale = Vector3.one * right.magnitude;
  }

  public void OnStateChagned(int state) {
    Logo.SetActive(state == 1);
    enabled = state == 1;
  }
}

}
