using UnityEngine;

public class BrowserWindow : MonoBehaviour
{
    public BrowserObject last_selected = null;

    public BrowserObject getLastSelected()
    {
        return last_selected;
    }

    public void setLastSelected(BrowserObject o)
    {
        last_selected = o;
    }
}
