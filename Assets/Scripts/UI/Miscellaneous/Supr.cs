using System;
using System.Threading;

public class Supr : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                int lcp = keyboard.window.lastCaretPosition;
                int lap = keyboard.window.lastAnchorPosition;
                keyboard.window.keyboardTarget.ActivateInputField();
                if (lcp == lap)
                {
                    keyboard.window.keyboardTarget.caretPosition = lcp;
                    keyboard.window.keyboardTarget.selectionAnchorPosition = lap;
                    keyboard.window.keyboardTarget.text = keyboard.window.keyboardTarget.text.Remove(lcp, 1);
                    keyboard.window.lastCaretPosition = lcp;
                    keyboard.window.lastAnchorPosition = lap;
                }
                else
                {
                    if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
                    keyboard.window.keyboardTarget.text =
                        keyboard.window.keyboardTarget.text.Remove(Math.Min(lcp, lap), lcp - lap);
                    keyboard.window.keyboardTarget.caretPosition = Math.Min(lcp, lap);
                    keyboard.window.keyboardTarget.selectionAnchorPosition = Math.Min(lcp, lap);
                    keyboard.window.lastCaretPosition = Math.Min(lcp, lap);
                    keyboard.window.lastAnchorPosition = Math.Min(lcp, lap);
                }
                keyboard.window.fromUIClick = true;
            }
            catch { }
        }
    }
}
