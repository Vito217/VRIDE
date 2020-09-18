using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Supr : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                int lcp = keyboard.window.lastCaretPosition;
                keyboard.window.keyboardTarget.ActivateInputField();
                keyboard.window.keyboardTarget.caretPosition = lcp;
                keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Remove(lcp, 1);
            }
            catch { }
        }
    }
}
