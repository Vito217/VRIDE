using UnityEngine;
using UnityEngine.UI;
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
        GetComponent<Button>().onClick.Invoke();
    }

    public virtual void onSelect() {
        GetComponent<TextMeshProUGUI>().color = theBrowser.skyBlue;
    }

    public void onDeselect() {
        GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }
}
