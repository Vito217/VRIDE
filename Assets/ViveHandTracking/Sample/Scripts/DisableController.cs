using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

class DisableController : MonoBehaviour {
  IEnumerator Start() {
#if VIVEHANDTRACKING_WITH_WAVEVR && !UNITY_EDITOR
    // wait until wavevr runtime is fully loaded
    yield return new WaitForSeconds(1);
    // disable controller interaction
    wvr.Interop.WVR_SetInteractionMode(wvr.WVR_InteractionMode.WVR_InteractionMode_Gaze);
#elif VIVEHANDTRACKING_WAVEXR_HAND
    // disable controller interaction
    Wave.Native.Interop.WVR_SetInteractionMode(Wave.Native.WVR_InteractionMode.WVR_InteractionMode_Gaze);
#endif

    GameObject.Destroy(this);
    yield break;
  }
}

}

