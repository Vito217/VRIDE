using System.Threading;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;

public class Copy : MonoBehaviour
{
    public void OnClick()
    {
        try
        {
            TMP_InputField target = EventSystem.current.currentSelectedGameObject.GetComponent<TMP_InputField>();

            if (target.interactable)
            {
                target.ActivateInputField();
                int start = target.selectionAnchorPosition;
                int end = target.caretPosition;
                if (end < start) start = Interlocked.Exchange(ref end, start);
                string selection = target.text.Substring(start, end - start);
                GUIUtility.systemCopyBuffer = selection;
            }
        }
        catch { }
    }
}
