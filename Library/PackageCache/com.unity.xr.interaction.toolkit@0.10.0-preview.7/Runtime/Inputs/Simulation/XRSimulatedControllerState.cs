using System.Runtime.InteropServices;
using UnityEngine.InputSystem.Layouts;
using UnityEngine.InputSystem.LowLevel;
using UnityEngine.InputSystem.Utilities;

namespace UnityEngine.XR.Interaction.Toolkit.Inputs.Simulation
{
    /// <summary>
    /// Button indices for <see cref="XRSimulatedControllerState.buttons"/>
    /// </summary>
    public enum ControllerButton
    {
        PrimaryButton,
        PrimaryTouch,
        SecondaryButton,
        SecondaryTouch,
        GripButton,
        TriggerButton,
        MenuButton,
        Primary2DAxisClick,
        Primary2DAxisTouch,
        Secondary2DAxisClick,
        Secondary2DAxisTouch,
        UserPresence,
    }

    [StructLayout(LayoutKind.Explicit, Size = 63)]
    public struct XRSimulatedControllerState : IInputStateTypeInfo
    {
        /// <summary>
        /// Memory format identifier for <see cref="XRSimulatedControllerState"/>.
        /// </summary>
        /// <seealso cref="InputStateBlock.format"/>
        public static FourCC formatId => new FourCC('X', 'R', 'S', 'C');

        /// <inheritdoc />
        public FourCC format => formatId;

        [InputControl(usage = "Primary2DAxis", aliases = new[] { "thumbstick", "joystick" })]
        [FieldOffset(0)]
        public Vector2 primary2DAxis;

        [InputControl(usage = "Trigger", layout = "Axis")]
        [FieldOffset(8)]
        public float trigger;

        [InputControl(usage = "Grip", layout = "Axis")]
        [FieldOffset(12)]
        public float grip;

        [InputControl(usage = "Secondary2DAxis")]
        [FieldOffset(16)]
        public Vector2 secondary2DAxis;

        [InputControl(name = nameof(XRSimulatedController.primaryButton), usage = "PrimaryButton", layout = "Button", bit = (uint)ControllerButton.PrimaryButton)]
        [InputControl(name = nameof(XRSimulatedController.primaryTouch), usage = "PrimaryTouch", layout = "Button", bit = (uint)ControllerButton.PrimaryTouch)]
        [InputControl(name = nameof(XRSimulatedController.secondaryButton), usage = "SecondaryButton", layout = "Button", bit = (uint)ControllerButton.SecondaryButton)]
        [InputControl(name = nameof(XRSimulatedController.secondaryTouch), usage = "SecondaryTouch", layout = "Button", bit = (uint)ControllerButton.SecondaryTouch)]
        [InputControl(name = nameof(XRSimulatedController.gripButton), usage = "GripButton", layout = "Button", bit = (uint)ControllerButton.GripButton, alias = "gripPressed")]
        [InputControl(name = nameof(XRSimulatedController.triggerButton), usage = "TriggerButton", layout = "Button", bit = (uint)ControllerButton.TriggerButton, alias = "triggerPressed")]
        [InputControl(name = nameof(XRSimulatedController.menuButton), usage = "MenuButton", layout = "Button", bit = (uint)ControllerButton.MenuButton)]
        [InputControl(name = nameof(XRSimulatedController.primary2DAxisClick), usage = "Primary2DAxisClick", layout = "Button", bit = (uint)ControllerButton.Primary2DAxisClick)]
        [InputControl(name = nameof(XRSimulatedController.primary2DAxisTouch), usage = "Primary2DAxisTouch", layout = "Button", bit = (uint)ControllerButton.Primary2DAxisTouch)]
        [InputControl(name = nameof(XRSimulatedController.secondary2DAxisClick), usage = "Secondary2DAxisClick", layout = "Button", bit = (uint)ControllerButton.Secondary2DAxisClick)]
        [InputControl(name = nameof(XRSimulatedController.secondary2DAxisTouch), usage = "Secondary2DAxisTouch", layout = "Button", bit = (uint)ControllerButton.Secondary2DAxisTouch)]
        [InputControl(name = nameof(XRSimulatedController.userPresence), usage = "UserPresence", layout = "Button", bit = (uint)ControllerButton.UserPresence)]
        [FieldOffset(24)]
        public ushort buttons;

        [InputControl(usage = "BatteryLevel", layout = "Axis")]
        [FieldOffset(26)]
        public float batteryLevel;

        [InputControl(usage = "TrackingState", layout = "Integer")]
        [FieldOffset(30)]
        public int trackingState;

        [InputControl(usage = "IsTracked", layout = "Button")]
        [FieldOffset(34)]
        public bool isTracked;

        [InputControl(usage = "DevicePosition")]
        [FieldOffset(35)]
        public Vector3 devicePosition;

        [InputControl(usage = "DeviceRotation")]
        [FieldOffset(47)]
        public Quaternion deviceRotation;

        /// <summary>
        /// Set the button mask for the given <paramref name="button"/>.
        /// </summary>
        /// <param name="button">Button whose state to set.</param>
        /// <param name="state">Whether to set the bit on or off.</param>
        /// <returns>The same <see cref="XRSimulatedControllerState"/> with the change applied.</returns>
        /// <seealso cref="buttons"/>
        public XRSimulatedControllerState WithButton(ControllerButton button, bool state = true)
        {
            var bit = 1 << (int)button;
            if (state)
                buttons |= (ushort)bit;
            else
                buttons &= (ushort)~bit;
            return this;
        }

        public void Reset()
        {
            primary2DAxis = default;
            trigger = default;
            grip = default;
            secondary2DAxis = default;
            buttons = default;
            batteryLevel = default;
            trackingState = default;
            isTracked = default;
            devicePosition = default;
            deviceRotation = Quaternion.identity;
        }
    }
}
