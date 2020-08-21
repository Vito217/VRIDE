using UnityEngine;
using TMPro;

public class BrowserPackage : BrowserObject
{
    public override async void onSelect()
    {
        theBrowser.DeactivateTemporarily();
        GetComponent<TextMeshProUGUI>().color = theBrowser.skyBlue;
        BrowserPackage last = theBrowser.package_list.getLastSelected() as BrowserPackage;
        if (last != null) last.onDeselect();
        theBrowser.package_list.setLastSelected(this);
        theBrowser.class_list.gameObject.SetActive(true);
        theBrowser.methodList.gameObject.SetActive(false);
        theBrowser.class_list.Load();
    }

    public override void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }
}
