using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class Letter : VRKey
{
    public override void OnClick()
    {
        if(keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            int lcp = keyboard.window.lastCaretPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                name
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + 1;
        }
    }
}
