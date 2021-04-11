using System.Collections;
using UnityEngine;

public class Copy : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            StartCoroutine(Copying());
        }
    }

    IEnumerator Copying()
    {
        int lcp = keyboard.window.keyboardTarget.caretPosition;
        int lap = keyboard.window.keyboardTarget.selectionAnchorPosition;
        keyboard.window.keyboardTarget.ActivateInputField();

        yield return null;

        string selection = keyboard.window.getSelectedCode(keyboard.window.field.text, true);
        GUIUtility.systemCopyBuffer = selection;
        keyboard.window.keyboardTarget.caretPosition = lcp;
        keyboard.window.keyboardTarget.selectionAnchorPosition = lap;
    }
}
