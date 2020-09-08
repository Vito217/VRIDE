using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AcceptKey : VRKey
{
    public void OnClick()
    {
        if (keyboard.window != null)
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
