using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AcceptKey : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                Browser b = keyboard.window as Browser;
                b.PharoDefine();
            }
            catch { }
        }
    }
}
