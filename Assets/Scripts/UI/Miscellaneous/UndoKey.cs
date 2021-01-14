using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class UndoKey : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
        }
    }
}
