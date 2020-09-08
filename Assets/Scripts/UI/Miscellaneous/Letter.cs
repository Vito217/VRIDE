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
            keyboard.window.field.ActivateInputField();
            keyboard.window.field.text = keyboard.window.field.text.Insert(
                lcp,
                name
            );
            keyboard.window.field.caretPosition = lcp + 1;
        }
    }
}
