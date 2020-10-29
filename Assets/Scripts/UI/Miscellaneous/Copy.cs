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
            int lcp = keyboard.window.lastCaretPosition;
            int lap = keyboard.window.lastAnchorPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            string selection = keyboard.window.getSelectedCode(keyboard.window.field.text, true);
            GUIUtility.systemCopyBuffer = selection;
            keyboard.window.keyboardTarget.caretPosition = lcp;
            keyboard.window.keyboardTarget.selectionAnchorPosition = lap;
            keyboard.window.lastCaretPosition = lcp;
            keyboard.window.lastAnchorPosition = lap;
            keyboard.window.fromUIClick = true;
        }
    }
}
