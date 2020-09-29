using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Paste : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            string selection = GUIUtility.systemCopyBuffer;
            int lcp = keyboard.window.lastCaretPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                selection
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + selection.Length;
        }
    }
}
