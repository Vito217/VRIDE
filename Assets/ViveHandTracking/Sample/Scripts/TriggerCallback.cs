using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

class TriggerCallback: MonoBehaviour {
  public int Value = 0;
  public System.Action<int> Callback = null;

  void OnTriggerEnter(Collider other) {
    if (Callback != null)
      Callback(Value);
  }
}

}
