using UnityEngine;
using System.Collections.Generic;
using UnityEngine.XR;

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

    public bool LeftTriggerDownFlag;
    public bool LeftTriggerUpFlag;
    public bool LeftPrimaryButtonUpFlag;
    public bool LeftPrimaryButtonDownFlag;
    public bool LeftSecondaryButtonUpFlag;
    public bool LeftSecondaryButtonDownFlag;

    public bool RightTriggerDownFlag;
    public bool RightTriggerUpFlag;
    public bool RightPrimaryButtonUpFlag;
    public bool RightPrimaryButtonDownFlag;
    public bool RightSecondaryButtonUpFlag;
    public bool RightSecondaryButtonDownFlag;

    public bool LeftAxisUpHoldFlag;
    public bool LeftAxisUpReleaseFlag;
    public bool RightAxisUpHoldFlag;
    public bool RightAxisUpReleaseFlag;

    public bool LeftAxisDownHoldFlag;
    public bool LeftAxisDownReleaseFlag;
    public bool RightAxisDownHoldFlag;
    public bool RightAxisDownReleaseFlag;

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
                device.IsPressed(InputHelpers.Button.Trigger, out LeftTrigger);
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
                device.IsPressed(InputHelpers.Button.PrimaryButton, out LeftPrimaryButton);
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
                device.IsPressed(InputHelpers.Button.SecondaryButton, out LeftSecondaryButton);
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
                device.IsPressed(InputHelpers.Button.PrimaryAxis2DUp, out LeftAxisUp);
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
                device.IsPressed(InputHelpers.Button.PrimaryAxis2DDown, out LeftAxisDown);
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
            }

            else if (device.characteristics.HasFlag(InputDeviceCharacteristics.Right))
            {

                device.IsPressed(InputHelpers.Button.Trigger, out RightTrigger);
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


                device.IsPressed(InputHelpers.Button.PrimaryButton, out RightPrimaryButton);
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


                device.IsPressed(InputHelpers.Button.SecondaryButton, out RightSecondaryButton);
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
                device.IsPressed(InputHelpers.Button.PrimaryAxis2DUp, out RightAxisUp);
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
                device.IsPressed(InputHelpers.Button.PrimaryAxis2DDown, out RightAxisDown);
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
