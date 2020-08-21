using UnityEngine;
using TMPro;

public class BrowserPackage : BrowserObject
{
    public override void onSelect()
    {
        BrowserPackage lastPackage = 
            theBrowser.package_list.getLastSelected() as BrowserPackage;

        if (lastPackage != null) lastPackage.onDeselect();
        theBrowser.package_list.setLastSelected(this);
        theBrowser.class_list.gameObject.SetActive(true);
        theBrowser.class_list.Load();

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public override void onDeselect()
    {
        foreach(Transform child in theBrowser.class_list.transform)
            if(child.gameObject.name != "template")
                Destroy(child.gameObject);

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
