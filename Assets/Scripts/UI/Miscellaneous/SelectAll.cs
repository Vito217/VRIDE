using TMPro;
using UnityEngine.EventSystems;

public class SelectAll : VRKey
{
    public override void OnClick()
    {
        try
        {
            TMP_InputField target = EventSystem.current.currentSelectedGameObject.GetComponent<TMP_InputField>();

            if (target.interactable)
            {
                target.ActivateInputField();
                target.onFocusSelectAll = true;
            }
        }
        catch { }
    }
}
