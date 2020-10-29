using System.Collections;
using System.Collections.Generic;
using System.Threading;
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
            int lap = keyboard.window.lastAnchorPosition;
            keyboard.window.keyboardTarget.ActivateInputField();
            if (lcp != lap)
            {
                if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
                keyboard.window.keyboardTarget.text = 
                    keyboard.window.keyboardTarget.text.Remove(Mathf.Min(lcp, lap), lcp - lap);
                keyboard.window.keyboardTarget.caretPosition = Mathf.Min(lcp, lap);
                lcp = Mathf.Min(lcp, lap);
            }
            keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Insert(
                lcp,
                selection
            );
            keyboard.window.keyboardTarget.caretPosition = lcp + selection.Length;
            keyboard.window.keyboardTarget.selectionAnchorPosition = lcp + selection.Length;
            keyboard.window.lastCaretPosition = lcp + selection.Length;
            keyboard.window.lastAnchorPosition = lcp + selection.Length;
            keyboard.window.fromUIClick = true;
        }
    }
}
