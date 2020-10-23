using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Copy : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            keyboard.window.keyboardTarget.ActivateInputField();
            string selection = keyboard.window.getSelectedCode(
                keyboard.window.cleanCode(keyboard.window.field.text), true);
            GUIUtility.systemCopyBuffer = selection;
        }
    }
}
