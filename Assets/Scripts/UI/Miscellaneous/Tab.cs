using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Tab : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            int lcp = Mathf.Min(keyboard.window.lastCaretPosition, keyboard.window.lastAnchorPosition);
            keyboard.window.keyboardTarget.ActivateInputField();
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                "    "
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + 4;
            keyboard.window.keyboardTarget.selectionAnchorPosition = lcp + 4;
            keyboard.window.lastCaretPosition = lcp + 4;
            keyboard.window.lastAnchorPosition = lcp + 4;
            keyboard.window.fromUIClick = true;
        }
    }
}
