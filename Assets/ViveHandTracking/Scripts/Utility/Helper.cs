using UnityEngine;

namespace ViveHandTracking {

#if !UNITY_2017_2_OR_NEWER && UNITY_EDITOR
// this is used by editor simulation engine and model renderer editor
public static class TransformUtils {
  public static Vector3 GetInspectorRotation(Transform t) {
    return t.localRotation.eulerAngles;
  }

  public static void SetInspectorRotation(Transform t, Vector3 r) {
    t.localRotation = Quaternion.Euler(r);
  }
}
#endif

}
