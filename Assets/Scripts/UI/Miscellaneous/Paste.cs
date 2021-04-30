using System.Threading;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;

public class Paste : MonoBehaviour
{
    public void OnClick()
    {
        try
        {
            TMP_InputField target = EventSystem.current.currentSelectedGameObject.GetComponent<TMP_InputField>();

            if (target.interactable)
            {
                target.ActivateInputField();
                string selection = GUIUtility.systemCopyBuffer;
                int lcp = target.caretPosition;
                int lap = target.selectionAnchorPosition;

                if (lcp != lap)
                {
                    if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
                    target.text = target.text.Remove(Mathf.Min(lcp, lap), lcp - lap);
                    target.caretPosition = Mathf.Min(lcp, lap);
                    lcp = Mathf.Min(lcp, lap);
                }

                target.text = target.text.Insert(lcp, selection);
                target.caretPosition = lcp + selection.Length;
                target.selectionAnchorPosition = lcp + selection.Length;
            }
        }
        catch { }
    }
}
