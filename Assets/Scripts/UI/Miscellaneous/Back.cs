using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using UnityEngine;

public class Back : VRKey
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
                    keyboard.window.keyboardTarget.text = 
                        keyboard.window.keyboardTarget.text.Remove(lcp - 1, 1);
                    keyboard.window.keyboardTarget.caretPosition = lcp - 1;
                }
                else
                {
                    if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
                    keyboard.window.keyboardTarget.text = 
                        keyboard.window.keyboardTarget.text.Remove(Math.Min(lcp, lap), lcp - lap);
                    keyboard.window.keyboardTarget.caretPosition = Math.Min(lcp, lap);
                }
            }
            catch { }
        }
    }
}
