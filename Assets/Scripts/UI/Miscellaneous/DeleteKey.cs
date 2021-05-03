using System;
using System.Threading;
using TMPro;

public class DeleteKey : VRKey
{
    public override void OnClick()
    {
        try
        {
            TMP_InputField target = Keyboards.lastSelectedTextField;
            int lcp = target.caretPosition;
            int lap = target.selectionAnchorPosition;

            if (lcp != lap)
                DeleteSelection(ref lcp, ref lap, target);
            else
            {
                if (lcp < target.text.Length)
                    target.text = target.text.Remove(lcp, 1);

                target.caretPosition = lcp;
                target.selectionAnchorPosition = lcp;
            }
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
