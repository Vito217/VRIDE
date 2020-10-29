using System.Threading;
using UnityEngine;

public class Enter : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            int lcp = keyboard.window.lastCaretPosition;
            int lap = keyboard.window.lastAnchorPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                "\n"
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + 1;
            keyboard.window.keyboardTarget.selectionAnchorPosition = lcp + 1;
            keyboard.window.lastCaretPosition = lcp + 1;
            keyboard.window.lastAnchorPosition = lcp + 1;
            keyboard.window.fromUIClick = true;
        }
    }
}
