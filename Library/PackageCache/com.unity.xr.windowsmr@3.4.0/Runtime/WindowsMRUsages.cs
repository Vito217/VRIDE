
using UnityEngine;
using UnityEngine.XR;

namespace Unity.XR.WindowsMR
{
    /// <summary>
    /// Input Usages, consumed by the UnityEngine.XR.InputDevice class in order to retrieve inputs.
    /// These usages are all WindowsMR specific.
    /// </summary>
    public static class WindowsMRUsages
    {
        /// <summary>
        /// A Vector3 position representing the tip of the controller pointing forward.
        /// </summary>
        public static InputFeatureUsage<Vector3> PointerPosition = new InputFeatureUsage<Vector3>("PointerPosition");
        /// <summary>
        ///  A Quaternion rotation representing the tip of the controller pointing forward.
        /// </summary>
        public static InputFeatureUsage<Quaternion> PointerRotation = new InputFeatureUsage<Quaternion>("PointerRotation");
        /// <summary>
        /// A [0,1] value that reports the risk that detection of the hand will be lost.
        /// </summary>
        public static InputFeatureUsage<float> SourceLossRisk = new InputFeatureUsage<float>("SourceLossRisk");
        /// <summary>
        /// A Vector3 direction that reports the suggested direction the user should move his hand if he is at risk of losing tracking.
        /// </summary>
        public static InputFeatureUsage<Vector3> SourceMitigationDirection = new InputFeatureUsage<Vector3>("SourceMitigationDirection");
        /// <summary>
        /// A bool representing if the user's index finger in the down position.
        /// </summary>
        /// <remarks>This data is HoloLens-specific and can also be queried using CommonUsages.triggerButton.</remarks>
        public static InputFeatureUsage<bool> AirTap = new InputFeatureUsage<bool>("AirTap");
        /// <summary>
        /// A Vector3 representing the direction that the users eyes are looking in.
        /// </summary>
        /// <remarks>This data is HoloLens2-specific.</remarks>
        public static InputFeatureUsage<Vector3> EyeGazePosition = new InputFeatureUsage<Vector3>("EyeGazePosition");
        /// <summary>
        /// A Vector3 representing the rotation to apply to Vector3.forward to generate the eye gaze direction.
        /// </summary>
        /// <remarks>This data is HoloLens2-specific.</remarks>
        public static InputFeatureUsage<Quaternion> EyeGazeRotation = new InputFeatureUsage<Quaternion>("EyeGazeRotation");
        /// <summary>
        /// A bool letting a user know eye gaze support is available. Always check this prior to actually gettin the eye gaze.
        /// This will be false on any platform that doesn't support eye gaze tracking.
        /// This may be false on platforms that do support eye gaze tracking but for one reason or another gaze information may not be available.
        /// </summary>
        /// <remarks>This data is HoloLens2-specific. </remarks>
        public static InputFeatureUsage<bool> EyeGazeAvailable = new InputFeatureUsage<bool>("EyeGazeAvailable");
    }
}
