using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;

namespace ViveHandTracking {

// Enum for selecting computation backend.
public enum GestureBackend {
  Auto = 0,  // default backend, use GPU on PC and CPU on Android, Recommended
  CPU = 1,   // use CPU, not supported on PC
  GPU = 2,   // use GPU, supported on PC/Android
}

// Enum for detection mode. Larger mode return more info, but runs more slowly. If a mode is not
// supported on a device, will fallback to previous supported mode.
public enum GestureMode {
  Point2D = 0,   // Fastest mode, return one 2d point for hand, supported on all devices
  Point3D = 1,   // Return one 3d point for hand, supported on dual camera devices
  Skeleton = 2,  // Return skeleton (21 points) for hand, supported on PC and WaveVR
}

[Serializable]
[StructLayout(LayoutKind.Sequential)]
public class GestureOption {
  public GestureBackend backend = GestureBackend.Auto;
  public GestureMode mode = GestureMode.Skeleton;
  [Range(15, 90)]
  [Tooltip("Limit max fps of raw detection. This has negative impact on latency. Only use it when VR application fps slows down due to hand tracking.")]
  public int maxFPS = 90; // limit max fps of detection
}

// Enum for predefined gesture classification
public enum GestureType {
  Unknown = 0,  // All other gestures not in predefined set
  Point = 1,
  Fist = 2,
  OK = 3,
  Like = 4,
  Five = 5,
  Victory = 6,
}

// Struct containing information of pinch
public struct PinchInfo {
  // Returns pinch level of the hand, within [0, 1], higher means more possible to pinch.
  // If you only need a boolean value for pinch or not, you can use isPinching instead.
  public float pinchLevel;

  // Returns if currently pinching or not.
  // If you need a range value within [0, 1], you can use pinchLevel instead.
  public bool isPinching {
    get {
      return pinchLevel > 0.7f;
    }
  }

  // Returns start position of the pinch ray.
  public Vector3 pinchStart;

  // Returns direction of the pinch.
  public Vector3 pinchDirection;

  // Returns rotation of the pinch ray. If only need a forward direction, you can use pinchDirection instead.
  public Quaternion pinchRotation {
    get {
      return Quaternion.FromToRotation(Vector3.forward, pinchDirection);
    }
  }
}

// Class containing detection result for one hand
[StructLayout(LayoutKind.Sequential)]
public class GestureResult {
  // Returns if this hand is left/right
  public bool isLeft {
    get;
    private set;
  }

  // Returns position of the hand. This field is guaranteed to be not null. The meaning of this
  // field is different based on actual GestureMode.
  // Point2D & Point3D: Only first point is used as the the position of hand.
  // Skeleton: The points is a 21-sized array with all the keypoints of the hand.
  public Vector3[] points {
    get;
    private set;
  }

  // Returns pre-defined gesture type.
  public GestureType gesture {
    get;
    private set;
  }

  // Returns confidence of the hand, within [0, 1].
  public float confidence {
    get;
    private set;
  }

  // Returns information of pinch (index and thumb) finger, includig pinch level and directions.
  public PinchInfo pinch {
    get;
    private set;
  }

  // Returns position of the hand. The meaning of this field is different based on actual GestureMode.
  // Point2D & Point3D: It's value is same as points[0].
  // Skeleton: This is calculated position of palm center.
  public Vector3 position {
    get;
    private set;
  }

  // Returns rotation of the hand. The meaning of this field is different based on actual GestureMode.
  // Point2D & Point3D: This is direction from camera position to points[0], up is always +y.
  // Skeleton: This is calculated rotation of palm.
  public Quaternion rotation {
    get;
    private set;
  }

  internal GestureResult(GestureResultRaw raw) {
    pinch = SetRaw(raw);
  }

  internal void Update(GestureResultRaw raw) {
    var newPinch = SetRaw(raw);
    // lerp pinch direction for stability
    newPinch.pinchDirection = Vector3.Lerp(pinch.pinchDirection, newPinch.pinchDirection, 5 * Time.deltaTime);
    pinch = newPinch;
  }

  private PinchInfo SetRaw(GestureResultRaw raw) {
    isLeft = raw.isLeft;
    gesture = raw.gesture;
    points = raw.points;
    confidence = raw.confidence;
    var pinchInfo = new PinchInfo();
    pinchInfo.pinchLevel = raw.pinchLevel;

    if (GestureProvider.HaveSkeleton) {
      Vector3 wrist = points[0], index = points[5], middle = points[9];
      Vector3 vec1 = index - wrist;
      Vector3 vec2 = middle - wrist;
      Vector3 forward = isLeft ? Vector3.Cross(vec1, vec2) : Vector3.Cross(vec2, vec1);

      position = (middle + wrist) / 2;
      rotation = Quaternion.LookRotation(forward, vec2);
      pinchInfo.pinchStart = (points[8] + points[4] * 3) / 4;
      var start = (GestureProvider.Current.transform.position + position) / 2;
      pinchInfo.pinchDirection = pinchInfo.pinchStart - start;
    } else {
      position = pinchInfo.pinchStart = points[0];
      pinchInfo.pinchDirection = position - GestureProvider.Current.transform.position;
      rotation = Quaternion.LookRotation(pinchInfo.pinchDirection);
    }
    return pinchInfo;
  }
}

[StructLayout(LayoutKind.Sequential)]
internal class GestureResultRaw {
  [MarshalAs (UnmanagedType.I1)]
  internal bool isLeft;

  [MarshalAs(UnmanagedType.ByValArray, SizeConst = 21)]
  internal Vector3[] points;

  internal GestureType gesture;

  internal float confidence;

  internal float pinchLevel;
}

// Enum for possible errors in gesture detection
public enum GestureFailure {
  None = 0,        // No error occurs
  OpenCL = -1,     // (Only on Windows) OpenCL is not supported on the machine
  Camera = -2,     // Start camera failed
  Internal = -10,  // Internal errors
  CPUOnPC = -11,   // CPU backend is not supported on Windows
};

// Enum for possible status in gesture detection
public enum GestureStatus {
  NotStarted = 0, // Detection is not started or stopped
  Starting = 1,   // Detection is started, but first result is not returned yet
  Running = 2,    // Detection is running and updates result regularly
  Error = 3,      // Detection failed to start, or error occured during detection
}

static class GestureInterface {
#if (VIVEHANDTRACKING_WITH_WAVEVR || VIVEHANDTRACKING_WAVEXR_HAND) && UNITY_ANDROID && !UNITY_EDITOR
  private const string DLLPath = "aristo_interface_wavevr";
#else
  private const string DLLPath = "aristo_interface";
#endif

  [DllImport(DLLPath)]
  internal static extern GestureFailure StartGestureDetection([In, Out] GestureOption option);

  [DllImport(DLLPath)]
  internal static extern void StopGestureDetection();

  [DllImport(DLLPath)]
  internal static extern int GetGestureResult(out IntPtr points, out int frameIndex);

  [DllImport(DLLPath)]
  internal static extern void UseExternalTransform([MarshalAs (UnmanagedType.I1)] bool value);

  [DllImport(DLLPath)]
  internal static extern void SetCameraTransform(Vector3 position, Quaternion rotation);
}

// Helper class for extension methods
public static class GestureHelper {
  // Test if a point in skeleton is valid or not. Can be used as point.IsValidGesturePoint().
  public static bool IsValidGesturePoint(this Vector3 point) {
    return point.x != 0 || point.y != 0 || point.z != 0;
  }
}

}
