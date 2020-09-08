using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class InspectItKey : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                Playground p = keyboard.window as Playground;
                p.PharoInspect();
            }
            catch { }
        }
    }
}
