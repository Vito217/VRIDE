using System;
using System.Collections;
using System.Runtime.InteropServices;
using UnityEngine;
#if UNITY_2018_3_OR_NEWER
using UnityEngine.Android;
#endif

namespace ViveHandTracking {

public class ViveHandTrackingEngine: HandTrackingEngine {
#if (VIVEHANDTRACKING_WITH_WAVEVR || VIVEHANDTRACKING_WITH_GOOGLEVR) && !UNITY_EDITOR
  private static string[] permissionNames = { "android.permission.CAMERA" };
#endif
  internal int lastIndex = -1;
  private bool noCameraScale = true;

  public override bool IsSupported() {
#if (UNITY_EDITOR_WIN && UNITY_STANDALONE_WIN) || (!UNITY_EDITOR && (UNITY_STANDALONE_WIN || UNITY_ANDROID))
    return true;
#else
    return false;
#endif
  }

  public override IEnumerator Setup() {
    lastIndex = -1;
    var transform = GestureProvider.Current.transform;
    var gameObject = GestureProvider.Current.gameObject;

#if UNITY_ANDROID && !UNITY_EDITOR
#if VIVEHANDTRACKING_WITH_WAVEVR
    // get camera permission
    var pmInstance = WaveVR_PermissionManager.instance;
    while (!pmInstance.isInitialized())
      yield return null;

    bool granting = true;
    bool hasPermission = pmInstance.isPermissionGranted(permissionNames[0]);
    WaveVR_PermissionManager.requestCompleteCallback callback = (results) => {
      granting = false;
      if (results.Count > 0 && results[0].Granted)
        hasPermission = true;
    };
    while (!hasPermission) {
      granting = true;
      pmInstance.requestPermissions(permissionNames, callback);
      while (granting)
        yield return null;
    }
#elif VIVEHANDTRACKING_WITH_GOOGLEVR
    // get camera permission using daydream API
    if (UnityEngine.VR.VRSettings.loadedDeviceName == "daydream") {
      var permissionRequester = GvrPermissionsRequester.Instance;
      bool granting = true;
      Action<GvrPermissionsRequester.PermissionStatus[]> callback = (results) => granting = false;
      while (!permissionRequester.IsPermissionGranted(permissionNames[0])) {
        granting = true;
        permissionRequester.RequestPermissions(permissionNames, callback);
        while (granting)
          yield return null;
      }
    }
#elif UNITY_2018_3_OR_NEWER
    // Unity 2018.3 or newer adds support for android runtime permission
    while (!Permission.HasUserAuthorizedPermission(Permission.Camera)) {
      Permission.RequestUserPermission(Permission.Camera);
      yield return null;
    }
#else
    while (!Application.HasUserAuthorization(UserAuthorization.WebCam))
      yield return Application.RequestUserAuthorization(UserAuthorization.WebCam);
#endif
#endif

    GestureInterface.UseExternalTransform(true);
    noCameraScale = Vector3.Distance(transform.localScale, Vector3.one) < 0.01f;
    yield break;
  }

  public override IEnumerator StartDetection(GestureOption option) {
#if (VIVEHANDTRACKING_WITH_WAVEVR || VIVEHANDTRACKING_WAVEXR_HAND) && UNITY_ANDROID && !UNITY_EDITOR
    // retry at most 5 times if camera is not ready
    int count = 5;
#else
    int count = 1;
#endif
    while (count > 0) {
      State.Error = GestureInterface.StartGestureDetection(option);
      if (State.Error == GestureFailure.Camera && count > 1) {
        Debug.LogError("Start camera failed, retrying...");
        yield return new WaitForSeconds(0.5f);
        count--;
        continue;
      } else if (State.Error != GestureFailure.None) {
        Debug.LogError("Start gesture detection failed: " + State.Error);
        State.Status = GestureStatus.Error;
      } else {
        State.Mode = option.mode;
        State.Status = GestureStatus.Starting;
      }
      break;
    }
  }

  public override void UpdateResult() {
    var transform = GestureProvider.Current.transform;
    if (noCameraScale) {
      // if no camera scale is involved, pass hmd local transform to native side
      // this makes hand position more stable when hmd moves
      GestureInterface.SetCameraTransform(transform.localPosition, transform.localRotation);
      transform = transform.parent;
    }

    IntPtr ptr;
    int index;
    var size = GestureInterface.GetGestureResult(out ptr, out index);

    if (index < 0) {
      Debug.LogError("Gesture detection stopped");
      State.Status = GestureStatus.Error;
      State.Error = GestureFailure.Internal;
      return;
    }
    if (index <= lastIndex)
      return;
    lastIndex = index;
    if (State.Status == GestureStatus.Starting)
      State.Status = GestureStatus.Running;

    if (size <= 0) {
      State.LeftHand = State.RightHand = null;
      return;
    }

    bool isLeft = false;
    var structSize = Marshal.SizeOf(typeof(GestureResultRaw));
    for (var i = 0; i < size; i++) {
      var gesture = (GestureResultRaw)Marshal.PtrToStructure(ptr, typeof(GestureResultRaw));
      ptr = new IntPtr(ptr.ToInt64() + structSize);
      if (transform != null) {
        for (int j = 0; j < 21; j++)
          gesture.points[j] = transform.TransformPoint(gesture.points[j]);
      }
      State.SetRaw(gesture);
      isLeft = gesture.isLeft;
    }
    if (size == 1) {
      if (isLeft)
        State.RightHand = null;
      else
        State.LeftHand = null;
    }
  }

  public override void StopDetection() {
    GestureInterface.StopGestureDetection();
    lastIndex = - 1;
  }

  public override string Description() {
    return "Default detection engine";
  }
}

}
