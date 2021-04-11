using System.Collections;
using System.Threading;
using UnityEngine;

public class Paste : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            StartCoroutine(Pasting());
        }
    }

    IEnumerator Pasting()
    {
        string selection = GUIUtility.systemCopyBuffer;
        int lcp = keyboard.window.keyboardTarget.caretPosition;
        int lap = keyboard.window.keyboardTarget.selectionAnchorPosition;
        keyboard.window.keyboardTarget.ActivateInputField();

        yield return null;

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
    }
}
