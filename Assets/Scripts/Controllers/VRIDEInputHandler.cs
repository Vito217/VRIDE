using UnityEngine;
using System.Collections.Generic;
using UnityEngine.XR;
using UnityEngine.XR.Interaction.Toolkit;

public class VRIDEInputHandler : MonoBehaviour
{
    List<UnityEngine.XR.InputDevice> inputDevices;

    public bool LeftAxisUp;
    public bool LeftAxisUpHold;
    public bool LeftAxisUpRelease;

    public bool RightAxisUp;
    public bool RightAxisUpHold;
    public bool RightAxisUpRelease;

    public bool LeftAxisDown;
    public bool LeftAxisDownHold;
    public bool LeftAxisDownRelease;

    public bool RightAxisDown;
    public bool RightAxisDownHold;
    public bool RightAxisDownRelease;

    public bool LeftTrigger;
    public bool RightTrigger;
    public bool LeftPrimaryButton;
    public bool RightPrimaryButton;
    public bool LeftSecondaryButton;
    public bool RightSecondaryButton;

    public bool LeftTriggerDown;
    public bool RightTriggerDown;
    public bool LeftPrimaryButtonDown;
    public bool RightPrimaryButtonDown;
    public bool LeftSecondaryButtonDown;
    public bool RightSecondaryButtonDown;

    public bool LeftTriggerUp;
    public bool RightTriggerUp;
    public bool LeftPrimaryButtonUp;
    public bool RightPrimaryButtonUp;
    public bool LeftSecondaryButtonUp;
    public bool RightSecondaryButtonUp;

    public bool LeftAxisLeft;
    public bool LeftAxisLeftRelease;
    public bool LeftAxisLeftHold;
    public bool LeftAxisRight;

    public bool LeftAxisRightHold;
    public bool LeftAxisRightRelease;
    public bool RightAxisLeft;
    public bool RightAxisLeftHold;
    public bool RightAxisLeftRelease;
    public bool RightAxisRight;

    public bool RightAxisRightHold;
    public bool RightAxisRightRelease;

    bool LeftTriggerDownFlag;
    bool LeftTriggerUpFlag;
    bool LeftPrimaryButtonUpFlag;
    bool LeftPrimaryButtonDownFlag;
    bool LeftSecondaryButtonUpFlag;
    bool LeftSecondaryButtonDownFlag;

    bool RightTriggerDownFlag;
    bool RightTriggerUpFlag;
    bool RightPrimaryButtonUpFlag;
    bool RightPrimaryButtonDownFlag;
    bool RightSecondaryButtonUpFlag;
    bool RightSecondaryButtonDownFlag;

    bool LeftAxisUpHoldFlag;
    bool LeftAxisUpReleaseFlag;
    bool RightAxisUpHoldFlag;
    bool RightAxisUpReleaseFlag;

    bool LeftAxisDownHoldFlag;
    bool LeftAxisDownReleaseFlag;
    bool RightAxisDownHoldFlag;
    bool RightAxisDownReleaseFlag;
    bool LeftAxisLeftReleaseFlag;
    bool LeftAxisLeftHoldFlag;
    bool LeftAxisRightReleaseFlag;
    bool LeftAxisRightHoldFlag;

    bool RightAxisLeftReleaseFlag;
    bool RightAxisLeftHoldFlag;

    bool RightAxisRightReleaseFlag;
    bool RightAxisRightHoldFlag;

    // Start is called before the first frame update
    void Start()
    {
        inputDevices = new List<UnityEngine.XR.InputDevice>();
    }

    // Update is called once per frame
    void FixedUpdate()
    {
        InputDevices.GetDevicesWithCharacteristics(
           InputDeviceCharacteristics.Controller & InputDeviceCharacteristics.TrackedDevice,
           inputDevices);        

        foreach (var device in inputDevices)
        {
            if (device.characteristics.HasFlag(InputDeviceCharacteristics.Left))
            {
                // LEFT TRIGGER
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.Trigger, out LeftTrigger);
                if (LeftTrigger)
                    UpdateFlag(
                        ref LeftTriggerUpFlag,
                        ref LeftTriggerDownFlag,
                        ref LeftTriggerDown);
                else
                    UpdateFlag(
                        ref LeftTriggerDownFlag, 
                        ref LeftTriggerUpFlag, 
                        ref LeftTriggerUp);


                // LEFT PRIMARY BUTTON
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryButton, out LeftPrimaryButton);
                if (LeftPrimaryButton)
                    UpdateFlag(
                        ref LeftPrimaryButtonUpFlag, 
                        ref LeftPrimaryButtonDownFlag, 
                        ref LeftPrimaryButtonDown);
                else
                    UpdateFlag(
                        ref LeftPrimaryButtonDownFlag, 
                        ref LeftPrimaryButtonUpFlag, 
                        ref LeftPrimaryButtonUp);


                // LEFT SECONDARY BUTTON
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.SecondaryButton, out LeftSecondaryButton);
                if (LeftSecondaryButton)
                    UpdateFlag(
                        ref LeftPrimaryButtonUpFlag, 
                        ref LeftSecondaryButtonDownFlag, 
                        ref LeftSecondaryButtonDown);
                else
                    UpdateFlag(
                        ref LeftSecondaryButtonDownFlag, 
                        ref LeftSecondaryButtonUpFlag, 
                        ref LeftSecondaryButtonUp);

                // LEFT AXIS UP
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DUp, out LeftAxisUp);
                if (LeftAxisUp)
                    UpdateFlag(
                        ref LeftAxisUpReleaseFlag,
                        ref LeftAxisUpHoldFlag,
                        ref LeftAxisUpHold);
                else
                    UpdateFlag(
                        ref LeftAxisUpHoldFlag,
                        ref LeftAxisUpReleaseFlag,
                        ref LeftAxisUpRelease);

                // LEFT AXIS DOWN
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DDown, out LeftAxisDown);
                if (LeftAxisDown)
                    UpdateFlag(
                        ref LeftAxisDownReleaseFlag,
                        ref LeftAxisDownHoldFlag,
                        ref LeftAxisDownHold);
                else
                    UpdateFlag(
                        ref LeftAxisDownHoldFlag,
                        ref LeftAxisDownReleaseFlag,
                        ref LeftAxisDownRelease);

                // LEFT AXIS LEFT
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DLeft, out LeftAxisLeft);
                if (LeftAxisLeft)
                    UpdateFlag(
                        ref LeftAxisLeftReleaseFlag,
                        ref LeftAxisLeftHoldFlag,
                        ref LeftAxisLeftHold);
                else
                    UpdateFlag(
                        ref LeftAxisLeftHoldFlag,
                        ref LeftAxisLeftReleaseFlag,
                        ref LeftAxisLeftRelease);

                // LEFT AXIS RIGHT
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DRight, out LeftAxisRight);
                if (LeftAxisRight)
                    UpdateFlag(
                        ref LeftAxisRightReleaseFlag,
                        ref LeftAxisRightHoldFlag,
                        ref LeftAxisRightHold);
                else
                    UpdateFlag(
                        ref LeftAxisRightHoldFlag,
                        ref LeftAxisRightReleaseFlag,
                        ref LeftAxisRightRelease);
            }

            else if (device.characteristics.HasFlag(InputDeviceCharacteristics.Right))
            {

                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.Trigger, out RightTrigger);
                if (RightTrigger)
                    UpdateFlag(
                        ref RightTriggerUpFlag, 
                        ref RightTriggerDownFlag, 
                        ref RightTriggerDown);
                else
                    UpdateFlag(
                        ref RightTriggerDownFlag, 
                        ref RightTriggerUpFlag, 
                        ref RightTriggerUp);


                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryButton, out RightPrimaryButton);
                if (RightPrimaryButton)
                    UpdateFlag(
                        ref RightPrimaryButtonUpFlag, 
                        ref RightPrimaryButtonDownFlag, 
                        ref RightPrimaryButtonDown);
                else
                    UpdateFlag(
                        ref RightPrimaryButtonDownFlag, 
                        ref RightPrimaryButtonUpFlag, 
                        ref RightPrimaryButtonUp);


                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.SecondaryButton, out RightSecondaryButton);
                if (RightSecondaryButton)
                    UpdateFlag(
                        ref RightPrimaryButtonUpFlag, 
                        ref RightSecondaryButtonDownFlag, 
                        ref RightSecondaryButtonDown);
                else
                    UpdateFlag(
                        ref RightSecondaryButtonDownFlag, 
                        ref RightSecondaryButtonUpFlag, 
                        ref RightSecondaryButtonUp);

                // RIGHT AXIS UP
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DUp, out RightAxisUp);
                if (RightAxisUp)
                    UpdateFlag(
                        ref RightAxisUpReleaseFlag,
                        ref RightAxisUpHoldFlag,
                        ref RightAxisUpHold);
                else
                    UpdateFlag(
                        ref RightAxisUpHoldFlag,
                        ref RightAxisUpReleaseFlag,
                        ref RightAxisUpRelease);

                // RIGHT AXIS DOWN
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DDown, out RightAxisDown);
                if (RightAxisDown)
                    UpdateFlag(
                        ref RightAxisDownReleaseFlag,
                        ref RightAxisDownHoldFlag,
                        ref RightAxisDownHold);
                else
                    UpdateFlag(
                        ref RightAxisDownHoldFlag,
                        ref RightAxisDownReleaseFlag,
                        ref RightAxisDownRelease);

                // RIGHT AXIS LEFT
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DLeft, out RightAxisLeft);
                if (RightAxisLeft)
                    UpdateFlag(
                        ref RightAxisLeftReleaseFlag,
                        ref RightAxisLeftHoldFlag,
                        ref RightAxisLeftHold);
                else
                    UpdateFlag(
                        ref RightAxisLeftHoldFlag,
                        ref RightAxisLeftReleaseFlag,
                        ref RightAxisLeftRelease);

                // RIGHT AXIS RIGHT
                device.IsPressed(UnityEngine.XR.Interaction.Toolkit.InputHelpers.Button.PrimaryAxis2DRight, out RightAxisRight);
                if (RightAxisRight)
                    UpdateFlag(
                        ref RightAxisRightReleaseFlag,
                        ref RightAxisRightHoldFlag,
                        ref RightAxisRightHold);
                else
                    UpdateFlag(
                        ref RightAxisRightHoldFlag,
                        ref RightAxisRightReleaseFlag,
                        ref RightAxisRightRelease);
            }
        }
    }
    public void UpdateFlag(ref bool prevFlag, ref bool flag, ref bool state)
    {
        prevFlag = false;
        if (!flag && !state) { flag = true; state = true; }
        else if (flag && state) state = false;
    }
}
