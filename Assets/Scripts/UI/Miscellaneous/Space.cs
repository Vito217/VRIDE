using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Space : VRKey
{
    public void OnClick()
    {
        if (keyboard.window != null)
        {
            int lcp = keyboard.window.lastCaretPosition;
            keyboard.window.field.ActivateInputField();
            keyboard.window.field.text = keyboard.window.field.text.Insert(
                lcp,
                " "
            );
            keyboard.window.field.caretPosition = lcp + 1;
        }
    }
}
