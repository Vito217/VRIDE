using System;
using System.Collections;
using System.Linq;
using UnityEngine;

namespace ViveHandTracking {

[Serializable]
class HandGameObject {
  public GameObject HandsRoot = null;
  public bool isModel = true;
}

class HandDisplaySwitch : MonoBehaviour {
  public HandGameObject[] Hands = null;
  private int index = 0;

  void Awake() {
    if (Hands == null)
      GameObject.Destroy(this);
    Hands = Hands.Where(o => o.HandsRoot != null).ToArray();
  }

  IEnumerator Start () {
    // wait until detection is started, so we know what mode we are using
    while (GestureProvider.Status == GestureStatus.NotStarted)
      yield return null;
    // never show model if in 2D/3D point mode
    if (!GestureProvider.HaveSkeleton)
      Hands = Hands.Where(o => !o.isModel).ToArray();
    if (index > Hands.Length)
      index = 0;
    if (Hands.Length > 0)
      SwitchDisplay(false);
    if (Hands.Length <= 1)
      GameObject.Destroy(this);
  }

  void Update () {
    if (Input.GetKeyDown(KeyCode.Space))
      SwitchDisplay();
  }

  void SwitchDisplay(bool next = true) {
    if (next)
      index = (index + 1) % Hands.Length;
    for (int i = 0; i < Hands.Length; i++)
      Hands[i].HandsRoot.SetActive(i == index);
  }

  public void OnStateChanged(int state) {
    if (state == 1)
      SwitchDisplay();
  }
}

}
