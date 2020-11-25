#if UNITY_INPUT_SYSTEM
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.XR;
using UnityEngine.InputSystem.Controls;
using UnityEngine.InputSystem.Layouts;
using UnityEngine.Scripting;

namespace UnityEngine.XR.WindowsMR.Input
{
    /// <summary>
    /// A Windows Mixed Reality XR headset.
    /// </summary>
    [Preserve]
    [InputControlLayout(displayName = "Windows MR HMD")]
    public class WMRHMD : XRHMD
    {
        [Preserve]
        [InputControl]
        public ButtonControl userPresence { get; private set; }
        [Preserve]
        [InputControl]
        public IntegerControl trackingState { get; private set; }
        [Preserve]
        [InputControl]
        public ButtonControl isTracked { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "HeadPosition" })]
        public Vector3Control devicePosition { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "HeadRotation" })]
        public QuaternionControl deviceRotation { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control leftEyePosition { get; private set; }
        [Preserve]
        [InputControl]
        public QuaternionControl leftEyeRotation { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control rightEyePosition { get; private set; }
        [Preserve]
        [InputControl]
        public QuaternionControl rightEyeRotation { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control centerEyePosition { get; private set; }
        [Preserve]
        [InputControl]
        public QuaternionControl centerEyeRotation { get; private set; }


        protected override void FinishSetup()
        {
            base.FinishSetup();

            userPresence = GetChildControl<ButtonControl>("userPresence");
            trackingState = GetChildControl<IntegerControl>("trackingState");
            isTracked = GetChildControl<ButtonControl>("isTracked");
            devicePosition = GetChildControl<Vector3Control>("devicePosition");
            deviceRotation = GetChildControl<QuaternionControl>("deviceRotation");
            leftEyePosition = GetChildControl<Vector3Control>("leftEyePosition");
            leftEyeRotation = GetChildControl<QuaternionControl>("leftEyeRotation");
            rightEyePosition = GetChildControl<Vector3Control>("rightEyePosition");
            rightEyeRotation = GetChildControl<QuaternionControl>("rightEyeRotation");
            centerEyePosition = GetChildControl<Vector3Control>("centerEyePosition");
            centerEyeRotation = GetChildControl<QuaternionControl>("centerEyeRotation");
        }
    }

    /// <summary>
    /// A Windows Mixed Reality XR controller.
    /// </summary>
    [Preserve]
    [InputControlLayout(displayName = "HoloLens Hand", commonUsages = new[] { "LeftHand", "RightHand" })]
    public class HololensHand : XRController
    {
        [Preserve]
        [InputControl]
        public IntegerControl trackingState { get; private set; }
        [Preserve]
        [InputControl]
        public ButtonControl isTracked { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripPosition" })]
        public Vector3Control devicePosition { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripOrientation" })]
        public QuaternionControl deviceRotation { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripVelocity" })]
        public Vector3Control deviceVelocity { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "triggerbutton" })]
        public ButtonControl airTap { get; private set; }
        [Preserve]
        [InputControl]
        public AxisControl sourceLossRisk { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control sourceLossMitigationDirection { get; private set; }

        protected override void FinishSetup()
        {
            base.FinishSetup();

            airTap = GetChildControl<ButtonControl>("airTap");
            trackingState = GetChildControl<IntegerControl>("trackingState");
            isTracked = GetChildControl<ButtonControl>("isTracked");
            devicePosition = GetChildControl<Vector3Control>("devicePosition");
            deviceRotation = GetChildControl<QuaternionControl>("deviceRotation");
            deviceVelocity = GetChildControl<Vector3Control>("deviceVelocity");
            sourceLossRisk = GetChildControl<AxisControl>("sourceLossRisk");
            sourceLossMitigationDirection = GetChildControl<Vector3Control>("sourceLossMitigationDirection");
        }
    }

    [Preserve]
    [InputControlLayout(displayName = "Windows MR Controller", commonUsages = new[] { "LeftHand", "RightHand" })]
    public class WMRSpatialController : XRControllerWithRumble
    {
        [Preserve]
        [InputControl(aliases = new[] { "Primary2DAxis", "thumbstickaxes" })]
        public Vector2Control joystick { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "Secondary2DAxis", "touchpadaxes" })]
        public Vector2Control touchpad { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripaxis" })]
        public AxisControl grip { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripbutton" })]
        public ButtonControl gripPressed { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "Primary", "menubutton" })]
        public ButtonControl menu { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "triggeraxis" })]
        public AxisControl trigger { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "triggerbutton" })]
        public ButtonControl triggerPressed { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "thumbstickpressed" })]
        public ButtonControl joystickClicked { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "joystickorpadpressed", "touchpadpressed" })]
        public ButtonControl touchpadClicked { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "joystickorpadtouched", "touchpadtouched" })]
        public ButtonControl touchpadTouched { get; private set; }
        [Preserve]
        [InputControl]
        public IntegerControl trackingState { get; private set; }
        [Preserve]
        [InputControl]
        public ButtonControl isTracked { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripPosition" })]
        public Vector3Control devicePosition { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripOrientation" })]
        public QuaternionControl deviceRotation { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripVelocity" })]
        public Vector3Control deviceVelocity { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "gripAngularVelocity" })]
        public Vector3Control deviceAngularVelocity { get; private set; }

        [Preserve]
        [InputControl]
        public AxisControl batteryLevel { get; private set; }
        [Preserve]
        [InputControl]
        public AxisControl sourceLossRisk { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control sourceLossMitigationDirection { get; private set; }
        [Preserve]
        [InputControl]
        public Vector3Control pointerPosition { get; private set; }
        [Preserve]
        [InputControl(aliases = new[] { "PointerOrientation" })]
        public QuaternionControl pointerRotation { get; private set; }

        protected override void FinishSetup()
        {
            base.FinishSetup();
            joystick = GetChildControl<Vector2Control>("joystick");
            trigger = GetChildControl<AxisControl>("trigger");
            touchpad = GetChildControl<Vector2Control>("touchpad");
            grip = GetChildControl<AxisControl>("grip");
            gripPressed = GetChildControl<ButtonControl>("gripPressed");
            menu = GetChildControl<ButtonControl>("menu");
            joystickClicked = GetChildControl<ButtonControl>("joystickClicked");
            triggerPressed = GetChildControl<ButtonControl>("triggerPressed");
            touchpadClicked = GetChildControl<ButtonControl>("touchpadClicked");
            touchpadTouched = GetChildControl<ButtonControl>("touchPadTouched");
            trackingState = GetChildControl<IntegerControl>("trackingState");
            isTracked = GetChildControl<ButtonControl>("isTracked");
            devicePosition = GetChildControl<Vector3Control>("devicePosition");
            deviceRotation = GetChildControl<QuaternionControl>("deviceRotation");
            deviceVelocity = GetChildControl<Vector3Control>("deviceVelocity");
            deviceAngularVelocity = GetChildControl<Vector3Control>("deviceAngularVelocity");

            batteryLevel = GetChildControl<AxisControl>("batteryLevel");
            sourceLossRisk = GetChildControl<AxisControl>("sourceLossRisk");
            sourceLossMitigationDirection = GetChildControl<Vector3Control>("sourceLossMitigationDirection");
            pointerPosition = GetChildControl<Vector3Control>("pointerPosition");
            pointerRotation = GetChildControl<QuaternionControl>("pointerRotation");
        }
    }
}
#endif //#if UNITY_INPUT_SYSTEM
