using TMPro;
using UnityEngine;

public class DesktopWindowObject : MonoBehaviour
{
    [HideInInspector]
    public DesktopWindowsExplorer explorer;

    [HideInInspector]
    public string hwnd;

    [HideInInspector]
    public string windowName;

    public virtual void onSelect()
    {
        if (explorer.lastSelected != null) explorer.lastSelected.onDeselect();
        explorer.lastSelected = this;

        // Instantiate a new window
        // --------------------------
        if(!GameObject.Find("ExtWindow:" + hwnd))
        {
            DesktopView dv = Instantiator.Instance.DesktopView(hwnd, windowName);
            dv.transform.position = transform.TransformPoint(0f, 0f, 0f);
            dv.transform.forward = transform.forward;
        }
        // --------------------------

        GetComponent<TextMeshProUGUI>().color = explorer.skyBlue;
    }

    public void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = explorer.white;
    }

    public void onEnterPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == explorer.skyBlue))
            GetComponent<TextMeshProUGUI>().color = explorer.gray;
    }

    public void onExitPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == explorer.skyBlue))
            GetComponent<TextMeshProUGUI>().color = explorer.white;
    }
}
