using UnityEngine;
using TMPro;

public class BrowserPackage : BrowserObject
{
    public PackageWindow parentWindow;
    public ClassWindow classList;

    public override void onSelect()
    {
        BrowserPackage lastPackage = parentWindow.getLastSelected() as BrowserPackage;
        if (lastPackage != null) lastPackage.onDeselect();
        parentWindow.setLastSelected(this);
        classList = Instantiator.Instance.ClassListObject(theBrowser.class_list, name, theBrowser);

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public override void onDeselect()
    {
        Color newCol;
        if (classList != null)
        {
            if (classList.last_selected != null)
                classList.last_selected.onDeselect();
            Destroy(classList.gameObject);
        }
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
