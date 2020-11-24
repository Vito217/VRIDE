using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

namespace ViveHandTracking {

public class EditorEngine : HandTrackingEngine {
  public override bool IsSupported() {
    return Application.isEditor;
  }

  public override string Description() {
    return "Simulate hand in Unity Editor, use Inspector to change hand pose & gesture";
  }

  public override IEnumerator Setup() {
    yield break;
  }

#if UNITY_EDITOR
  private EditorHandSimulator controller = null;

  public override IEnumerator StartDetection(GestureOption option) {
    if (State.Status == GestureStatus.Starting || State.Status == GestureStatus.Running)
      yield break;

    if (State.Mode == GestureMode.Point2D)
      State.Mode = GestureMode.Point3D;
    State.Status = GestureStatus.Running;

    var go = new GameObject("Editor Engine Controller");
    Selection.activeGameObject = go;
    controller = go.AddComponent<EditorHandSimulator>();
    controller.hasSkeleton = State.Mode == GestureMode.Skeleton;
  }

  public override void UpdateResult() {
    if (controller == null)
      return;
    if (controller.LeftHand != null)
      State.SetRaw(controller.LeftHand);
    else
      State.LeftHand = null;
    if (controller.RightHand != null)
      State.SetRaw(controller.RightHand);
    else
      State.RightHand = null;
  }

  public override void StopDetection() {
    if (controller != null) {
      GameObject.Destroy(controller.gameObject);
      controller = null;
    }
  }
#else
  public override IEnumerator StartDetection(GestureOption option) {
    yield break;
  }
  public override void UpdateResult() {}
  public override void StopDetection() {}
#endif
}

}
