using UnityEngine;
using Valve.VR;

public class TrackedControllerScript : MonoBehaviour
{
    public uint controllerIndex;
	uint controllerStateSize;
    public VRControllerState_t controllerState;
	private bool triggerPressed = false;
    private bool menuPressed = false;
 
    void Start()
    {
        if (this.GetComponent<SteamVR_TrackedObject>() == null)
        {
            gameObject.AddComponent<SteamVR_TrackedObject>();
        }

		if (controllerIndex != 0)
		{
			this.GetComponent<SteamVR_TrackedObject>().index = (SteamVR_TrackedObject.EIndex)controllerIndex;
			if (this.GetComponent<SteamVR_RenderModel>() != null)
			{
				this.GetComponent<SteamVR_RenderModel>().index = (SteamVR_TrackedObject.EIndex)controllerIndex;
			}
		}
		else
		{
			controllerIndex = (uint) this.GetComponent<SteamVR_TrackedObject>().index;
        }
    }

	public void SetDeviceIndex(int index)
	{
			this.controllerIndex = (uint) index;
	}

	void Update()
    {
		var system = OpenVR.System;
		if (system != null && system.GetControllerState(controllerIndex, ref controllerState, controllerStateSize))
		{
			ulong trigger = controllerState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Trigger));
            if (trigger > 0L && !triggerPressed)
            {
                triggerPressed = true;

				// todo: handle trigger
				Debug.Log("pulled trigger on device "+controllerIndex);

            }
            else if (trigger == 0L && triggerPressed)
            {
                triggerPressed = false;
            }

            ulong menu = controllerState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_ApplicationMenu));
            if (menu > 0L && !menuPressed)
            {
                menuPressed = true;

				GameObject eye = GameObject.Find("Camera (eye)");

				GameObject obj = eye.transform.Find("camview").gameObject;

				if(!obj.activeInHierarchy) {
					obj.SetActive(true);
				} else {
					obj.SetActive(false);
				}
						
			}
            else if (menu == 0L && menuPressed)
            {
                menuPressed = false;
            }
        }
    }
}
