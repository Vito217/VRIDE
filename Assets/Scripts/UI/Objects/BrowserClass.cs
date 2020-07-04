using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using TMPro;
using InstantiatorModule;

public class BrowserClass : BrowserObject
{
    public ClassWindow parent_window;
    public Transform classMethodList;
    public Transform instanceMethodList;

    void Start()
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

    public void onSelectClass()
    {
        field.text = sourceCode;
        BrowserClass last_class = parent_window.getLastSelectedClass();
        if(last_class != null) last_class.onDeselectClass();
        parent_window.setLastSelectedClass(this);

        classMethodList = Instantiator.MethodListObject(theBrowser.classSideList, name, field);
        instanceMethodList = Instantiator.MethodListObject(theBrowser.instanceSideList, name, field);

        foreach (Tuple<string, string, string> methodAndCode in 
            VRIDEController.sysData.data[parent_window.gameObject.name][name].Item2)
        {
            string methodName = methodAndCode.Item1;
            string methodCode = methodAndCode.Item2;
            string side = methodAndCode.Item3;

            if (side == "ClassSide")
                Instantiator.MethodObject(classMethodList, name, methodName, field, methodCode);
            else
                Instantiator.MethodObject(instanceMethodList, name, methodName, field, methodCode);
        }
        LayoutRebuilder.ForceRebuildLayoutImmediate(classMethodList.gameObject.GetComponent<RectTransform>());
        LayoutRebuilder.ForceRebuildLayoutImmediate(instanceMethodList.gameObject.GetComponent<RectTransform>());

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public void onDeselectClass()
    {
        Color newCol;
        if (classMethodList != null) Destroy(classMethodList.gameObject);
        if (instanceMethodList != null) Destroy(instanceMethodList.gameObject);
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
