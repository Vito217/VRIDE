using System.Collections;
using UnityEngine;
#if !UNITY_EDITOR && UNITY_ANDROID
#if VIVEHANDTRACKING_WAVEXR_HAND
using UnityEngine.XR;
using Wave.XR;
using Wave.Native;
#elif VIVEHANDTRACKING_WAVEVR_HAND
using wvr;
using RigidTransform = WaveVR_Utils.RigidTransform;
#endif
#endif

namespace ViveHandTracking {

public class WaveVRHandEngine: HandTrackingEngine {

#if (VIVEHANDTRACKING_WAVEVR_HAND || VIVEHANDTRACKING_WAVEXR_HAND) && !UNITY_EDITOR && UNITY_ANDROID

  private WVR_HandGestureData_t gestureData = new WVR_HandGestureData_t();
  private WVR_HandSkeletonData_t skeletonData = new WVR_HandSkeletonData_t();
  private WVR_HandPoseData_t poseData = new WVR_HandPoseData_t();
  private RigidTransform rigidTransform = RigidTransform.identity;
  private GestureResultRaw leftHand, rightHand;

  public override bool IsSupported() {
    return true;
  }

  public override IEnumerator Setup() {
    var transform = GestureProvider.Current.transform;
    var gameObject = GestureProvider.Current.gameObject;

    leftHand = CreateHand(true);
    rightHand = CreateHand(false);
    yield break;
  }

  public override IEnumerator StartDetection(GestureOption option) {
    if (State.Status == GestureStatus.Starting || State.Status == GestureStatus.Running)
      yield break;

    ulong feature = Interop.WVR_Base.Instance.GetSupportedFeatures();
    if ((feature & (ulong)WVR_SupportedFeature.WVR_SupportedFeature_HandGesture) == 0) {
      Debug.LogError("WaveVR gesture not supported");
      State.Status = GestureStatus.Error;
      yield break;
    }
    if ((feature & (ulong)WVR_SupportedFeature.WVR_SupportedFeature_HandTracking) == 0) {
      Debug.LogError("WaveVR tracking not supported");
      State.Status = GestureStatus.Error;
      yield break;
    }

    State.Status = GestureStatus.Starting;

    var result = Interop.WVR_Base.Instance.StartHandGesture();
    if (result != WVR_Result.WVR_Success) {
      Debug.LogError("WaveVR gesture start failed: " + result);
      State.Status = GestureStatus.Error;
      yield break;
    }
    result = Interop.WVR_Base.Instance.StartHandTracking();
    if (result != WVR_Result.WVR_Success) {
      Debug.LogError("WaveVR tracking start failed: " + result);
      State.Status = GestureStatus.Error;
      Interop.WVR_Base.Instance.StopHandGesture();
      yield break;
    }

    State.Mode = GestureMode.Skeleton;
    State.Status = GestureStatus.Running;
  }

  public override void UpdateResult() {
    if (State.Status != GestureStatus.Running)
      return;

#if VIVEHANDTRACKING_WAVEXR_HAND
    WVR_PoseOriginModel origin = WVR_PoseOriginModel.WVR_PoseOriginModel_OriginOnHead;
    var unityOrigin = Utils.InputSubsystem.GetTrackingOriginMode();
    if (unityOrigin == TrackingOriginModeFlags.Floor)
      origin = WVR_PoseOriginModel.WVR_PoseOriginModel_OriginOnGround;
    else if (unityOrigin == TrackingOriginModeFlags.TrackingReference)
      origin = WVR_PoseOriginModel.WVR_PoseOriginModel_OriginOnTrackingObserver;
#else
    WVR_PoseOriginModel origin = WaveVR_Render.Instance.origin;
#endif
    var result = Interop.WVR_Base.Instance.GetHandTrackingData(ref skeletonData, ref poseData, origin);
    if (result != WVR_Result.WVR_Success) {
      Debug.LogError("Get tracking data failed: " + result);
      State.Status = GestureStatus.Error;
      State.Error = GestureFailure.Internal;
      return;
    }

    result = Interop.WVR_Base.Instance.GetHandGestureData(ref gestureData);
    if (result != WVR_Result.WVR_Success) {
      Debug.LogError("Get gesture data failed: " + result);
      State.Status = GestureStatus.Error;
      State.Error = GestureFailure.Internal;
      return;
    }

    if (skeletonData.left.wrist.IsValidPose) {
      leftHand.gesture = MapGesture(gestureData.left);
      SetHandPoints(leftHand, skeletonData.left, poseData.left);
      State.SetRaw(leftHand);
    } else
      State.LeftHand = null;
    if (skeletonData.right.wrist.IsValidPose) {
      rightHand.gesture = MapGesture(gestureData.right);
      SetHandPoints(rightHand, skeletonData.right, poseData.right);
      State.SetRaw(rightHand);
    } else
      State.RightHand = null;
  }

  public override void StopDetection() {
    Interop.WVR_Base.Instance.StopHandTracking();
    Interop.WVR_Base.Instance.StopHandGesture();
  }

  GestureResultRaw CreateHand(bool left) {
    var hand = new GestureResultRaw();
    hand.isLeft = left;
    hand.points = new Vector3[21];
    return hand;
  }

  GestureType MapGesture(WVR_HandGestureType gesture) {
    switch (gesture) {
    case WVR_HandGestureType.WVR_HandGestureType_Fist:
      return GestureType.Fist;
    case WVR_HandGestureType.WVR_HandGestureType_Five:
      return GestureType.Five;
    case WVR_HandGestureType.WVR_HandGestureType_OK:
      return GestureType.OK;
    case WVR_HandGestureType.WVR_HandGestureType_ThumbUp:
      return GestureType.Like;
    case WVR_HandGestureType.WVR_HandGestureType_IndexUp:
      return GestureType.Point;
    default:
      return GestureType.Unknown;
    }
  }

  void SetHandPoints(GestureResultRaw hand, WVR_HandSkeletonState_t skeleton, WVR_HandPoseState_t pose) {
    if (hand == null || !skeleton.wrist.IsValidPose)
      return;
    rigidTransform.update(skeleton.wrist.PoseMatrix);
    hand.points[0] = rigidTransform.pos;
    SetFingerPoints(hand, skeleton.thumb, 1);
    SetFingerPoints(hand, skeleton.index, 5);
    SetFingerPoints(hand, skeleton.middle, 9);
    SetFingerPoints(hand, skeleton.ring, 13);
    SetFingerPoints(hand, skeleton.pinky, 17);

    // calculate pinch level
    if (pose.state.type == WVR_HandPoseType.WVR_HandPoseType_Pinch)
      hand.pinchLevel = pose.pinch.strength;
    else
      hand.pinchLevel = 0;
    hand.confidence = skeleton.confidence;

    // apply camera offset to hand points
    var transform = GestureProvider.Current.transform;
    if (transform.parent != null) {
      for (int i = 0; i < 21; i++)
        hand.points[i] = transform.parent.TransformPoint(hand.points[i]);
    }
  }

  void SetFingerPoints(GestureResultRaw hand, WVR_FingerState_t finger, int startIndex) {
#if VIVEHANDTRACKING_WAVEXR_HAND
    hand.points[startIndex] = Coordinate.GetVectorFromGL(finger.joint1);
    hand.points[startIndex + 1] = Coordinate.GetVectorFromGL(finger.joint2);
    hand.points[startIndex + 2] = Coordinate.GetVectorFromGL(finger.joint3);
    hand.points[startIndex + 3] = Coordinate.GetVectorFromGL(finger.tip);
#else
    hand.points[startIndex] = WaveVR_Utils.GetPosition(finger.joint1);
    hand.points[startIndex + 1] = WaveVR_Utils.GetPosition(finger.joint2);
    hand.points[startIndex + 2] = WaveVR_Utils.GetPosition(finger.joint3);
    hand.points[startIndex + 3] = WaveVR_Utils.GetPosition(finger.tip);
#endif
  }

#else

  public override bool IsSupported() {
    return false;
  }

  public override IEnumerator Setup() {
    yield break;
  }

  public override IEnumerator StartDetection(GestureOption option) {
    yield break;
  }

  public override void UpdateResult() {}

  public override void StopDetection() {}

  public override string Description() {
#if !UNITY_ANDROID
    return "[Experimental] Only supported on Android WaveVR device";
#elif VIVEHANDTRACKING_WAVEVR_HAND || VIVEHANDTRACKING_WAVEXR_HAND
    return "[Experimental] Requires real WaveVR device";
#else
    return "[Experimental] Requires WaveVR 3.2.0 (must import Wave.Native package if using WaveXR)";
#endif
  }

#endif

}
}
