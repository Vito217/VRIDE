using System;
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
        if (lastPackage != null) lastPackage.onDeselectPackage();
        parentWindow.setLastSelectedPackage(this);

        classList = Instantiator.Instance.ClassListObject(theBrowser.class_list, name, field);
        foreach (KeyValuePair<string, Tuple<string, List<Tuple<string, string, string>>>>
                         keyVal in VRIDEController.sysData.data[name])
        {
            string className = keyVal.Key;
            string classCode = keyVal.Value.Item1;
            BrowserClass c = Instantiator.Instance.ClassObject(classList, className, field, 
                null, null, classCode, theBrowser);
        }
        LayoutRebuilder.ForceRebuildLayoutImmediate(classList.gameObject.GetComponent<RectTransform>());

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public void onDeselectPackage()
    {
        Color newCol;
        if (classList != null)
        {
            if (classList.last_selected_class != null)
                classList.last_selected_class.onDeselectClass();
            Destroy(classList.gameObject);
        }
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
