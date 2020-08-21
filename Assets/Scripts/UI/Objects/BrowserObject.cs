using UnityEngine;
using UnityEngine.UI;

public class BrowserObject : MonoBehaviour
{
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
