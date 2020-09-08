using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Supr : VRKey
{
    public void OnClick()
    {
        if (keyboard.window != null)
        {
            try
            {
                int lcp = keyboard.window.lastCaretPosition;
                keyboard.window.field.ActivateInputField();
                keyboard.window.field.caretPosition = lcp;
                keyboard.window.field.text = keyboard.window.field.text.Remove(lcp, 1);
            }
            catch { }
        }
    }
}
