using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class BrowserObject : MonoBehaviour
{
    public string name;
    public string sourceCode;
    public Browser theBrowser;

    void Start()
    {
        innerStart();
    }

    public void click()
    {
        GetComponent<Button>().onClick.Invoke();
    }

    public virtual void innerStart() { }

    public virtual void onSelect() { }

    public virtual void onDeselect() { }
}
