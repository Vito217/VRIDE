using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DoItKey : VRKey
{
    public void OnClick()
    {
        if (keyboard.window != null)
        {
            try
            {
                Playground p = keyboard.window as Playground;
                p.PharoDo();
            }
            catch { }
        }
    }
}
