using System.Collections;

public class SelectAll : VRKey
{
    public override void OnClick()
    {
        StartCoroutine(SelectingAll());
    }

    IEnumerator SelectingAll()
    {
        keyboard.window.keyboardTarget.onFocusSelectAll = true;
        keyboard.window.keyboardTarget.ActivateInputField();

        yield return null;

        keyboard.window.keyboardTarget.onFocusSelectAll = false;
    }
}
