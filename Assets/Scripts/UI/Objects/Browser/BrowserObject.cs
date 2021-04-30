using UnityEngine;
using TMPro;

public class BrowserObject : MonoBehaviour
{
    public Browser theBrowser;

    void Start()
    {
        GetComponent<TextMeshProUGUI>().text = name;
    }

    public void click()
    {
        onSelect();
    }

    public virtual void onSelect() {
        GetComponent<TextMeshProUGUI>().color = theBrowser.skyBlue;
    }

    public void onDeselect() {
        GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }

    public void onEnterPointer()
    {
        if(!(GetComponent<TextMeshProUGUI>().color == theBrowser.skyBlue))
            GetComponent<TextMeshProUGUI>().color = theBrowser.gray;
    }

    public void onExitPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == theBrowser.skyBlue))
            GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }
}
