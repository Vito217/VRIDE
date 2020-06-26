using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using TMPro;

public class BrowserPackage : BrowserObject
{
    public PackageWindow parentWindow;
    public ClassWindow classList;

    public void onSelectPackage()
    {
        BrowserPackage lastPackage = parentWindow.getLastSelectedPackage();
        if (lastPackage != null)
            lastPackage.onDeselectPackage();
        parentWindow.setLastSelectedPackage(this);
        classList.gameObject.SetActive(true);
        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public void onDeselectPackage()
    {
        if (classList.getLastSelectedClass() != null)
            classList.getLastSelectedClass().onDeselectClass();
        classList.gameObject.SetActive(false);
        Color newCol;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
