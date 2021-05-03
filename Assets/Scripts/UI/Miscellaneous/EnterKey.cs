using System;
using System.Threading;
using TMPro;

public class EnterKey : VRKey
{
    public override void OnClick()
    {
        try
        {
            TMP_InputField target = Keyboards.lastSelectedTextField;
            int lcp = target.caretPosition;
            int lap = target.selectionAnchorPosition;
            if (lcp != lap) DeleteSelection(ref lcp, ref lap, target);
            target.text = target.text.Insert(lcp, "\n");
            target.caretPosition = lcp + 1;
            target.selectionAnchorPosition = lap + 1;
        }
        catch { }
    }

    private void DeleteSelection(ref int lcp, ref int lap, TMP_InputField target)
    {
        if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
        target.text = target.text.Remove(Math.Min(lcp, lap), lcp - lap);
        target.caretPosition = Math.Min(lcp, lap);
        lcp = Math.Min(lcp, lap);
    }
}
