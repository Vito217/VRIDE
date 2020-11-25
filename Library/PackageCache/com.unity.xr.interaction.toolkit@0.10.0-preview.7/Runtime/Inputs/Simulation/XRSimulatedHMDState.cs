using System.Runtime.InteropServices;
using UnityEngine.InputSystem.Layouts;
using UnityEngine.InputSystem.LowLevel;
using UnityEngine.InputSystem.Utilities;

namespace UnityEngine.XR.Interaction.Toolkit.Inputs.Simulation
{
    [StructLayout(LayoutKind.Explicit, Size = 117)]
    public struct XRSimulatedHMDState : IInputStateTypeInfo
    {
        /// <summary>
        /// Memory format identifier for <see cref="XRSimulatedHMDState"/>.
        /// </summary>
        /// <seealso cref="InputStateBlock.format"/>
        public static FourCC formatId => new FourCC('X', 'R', 'S', 'H');

        /// <inheritdoc />
        public FourCC format => formatId;

        [InputControl(usage = "LeftEyePosition")]
        [FieldOffset(0)]
        public Vector3 leftEyePosition;

        [InputControl(usage = "LeftEyeRotation")]
        [FieldOffset(12)]
        public Quaternion leftEyeRotation;

        [InputControl(usage = "RightEyePosition")]
        [FieldOffset(28)]
        public Vector3 rightEyePosition;

        [InputControl(usage = "RightEyeRotation")]
        [FieldOffset(40)]
        public Quaternion rightEyeRotation;

        [InputControl(usage = "CenterEyePosition")]
        [FieldOffset(56)]
        public Vector3 centerEyePosition;

        [InputControl(usage = "CenterEyeRotation")]
        [FieldOffset(68)]
        public Quaternion centerEyeRotation;

        [InputControl(usage = "TrackingState", layout = "Integer")]
        [FieldOffset(84)]
        public int trackingState;

        [InputControl(usage = "IsTracked", layout = "Button")]
        [FieldOffset(88)]
        public bool isTracked;

        [InputControl(usage = "DevicePosition")]
        [FieldOffset(89)]
        public Vector3 devicePosition;

        [InputControl(usage = "DeviceRotation")]
        [FieldOffset(101)]
        public Quaternion deviceRotation;

        public void Reset()
        {
            leftEyePosition = default;
            leftEyeRotation = Quaternion.identity;
            rightEyePosition = default;
            rightEyeRotation = Quaternion.identity;
            centerEyePosition = default;
            centerEyeRotation = Quaternion.identity;
            trackingState = default;
            isTracked = default;
            devicePosition = default;
            deviceRotation = Quaternion.identity;
        }
    }
}
