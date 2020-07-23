using System;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class BrowserClass : BrowserObject
{
    public ClassWindow parent_window;
    public MethodWindow classMethodList;
    public MethodWindow instanceMethodList;

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
            string package = parent_window.gameObject.name;

            classMethodList = Instantiator.Instance.MethodListObject(
                theBrowser.classSideList, name, theBrowser, "ClassSide", package);

            instanceMethodList = Instantiator.Instance.MethodListObject(
                theBrowser.instanceSideList, name, theBrowser, "InstanceSide", package);

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
