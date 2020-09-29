using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Enter : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            int lcp = keyboard.window.lastCaretPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                "\n"
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + 1;
        }
    }
}
