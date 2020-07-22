using System;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class BrowserClass : BrowserObject
{
    public ClassWindow parent_window;
    public Transform classMethodList;
    public Transform instanceMethodList;

    public override void innerStart()
    {
        if (sourceCode == "")
        {
            sourceCode =
                "<b>Object</b> subclass: #NameOfSubclass\n" +
                    "    instanceVariableNames: ''\n" +
                    "    classVariableNames: ''\n" +
                    "    package: 'MyPackage'";
        }
    }

    public override void onSelect()
    {
        theBrowser.field.text = sourceCode;
        BrowserClass last_class = parent_window.getLastSelected() as BrowserClass;
        if (last_class != null) last_class.onDeselect();
        parent_window.setLastSelected(this);
        if (name != "template")
        {
            classMethodList = Instantiator.Instance.MethodListObject(theBrowser.classSideList, name, theBrowser);
            instanceMethodList = Instantiator.Instance.MethodListObject(theBrowser.instanceSideList, name, theBrowser);

            foreach ((string methodName, string methodCode, string side) methodAndCode in
                VRIDEController.sysData.data[parent_window.gameObject.name][name].classMethods)
            {
                string methodName = methodAndCode.methodName;
                string methodCode = methodAndCode.methodCode;
                string side = methodAndCode.side;

                if (side == "ClassSide")
                    Instantiator.Instance.MethodObject(classMethodList, name, methodName, 
                        theBrowser.field, methodCode, theBrowser);
                else
                    Instantiator.Instance.MethodObject(instanceMethodList, name, methodName, 
                        theBrowser.field, methodCode, theBrowser);
            }
            LayoutRebuilder.ForceRebuildLayoutImmediate(classMethodList.gameObject.GetComponent<RectTransform>());
            LayoutRebuilder.ForceRebuildLayoutImmediate(instanceMethodList.gameObject.GetComponent<RectTransform>());
        }
        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public override void onDeselect()
    {
        Color newCol;
        if (classMethodList != null) Destroy(classMethodList.gameObject);
        if (instanceMethodList != null) Destroy(instanceMethodList.gameObject);
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
