using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
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

        classList = Instantiator.Instance.ClassListObject(theBrowser.class_list, name, field);
        foreach (KeyValuePair<string, (string classCode, 
            List<(string methodName, string methodCode, string side)> classMethods)>
                keyVal in VRIDEController.sysData.data[name])
        {
            string className = keyVal.Key;
            string classCode = keyVal.Value.classCode;
            BrowserClass c = Instantiator.Instance.ClassObject(classList, className, field, 
                null, null, classCode, theBrowser);
        }
        LayoutRebuilder.ForceRebuildLayoutImmediate(classList.gameObject.GetComponent<RectTransform>());

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
