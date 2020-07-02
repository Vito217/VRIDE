using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using TMPro;

public class BrowserClass : BrowserObject
{
    public ClassWindow parent_window;
    public GameObject classMethodList;
    public GameObject instanceMethodList;

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
        if(last_class != null)
            last_class.onDeselectClass();
        parent_window.setLastSelectedClass(this);
        if(classMethodList != null)
            classMethodList.SetActive(true);
        if (instanceMethodList != null)
            instanceMethodList.SetActive(true);
        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public void onDeselectClass()
    {
        if (classMethodList != null)
            classMethodList.SetActive(false);
        if (instanceMethodList != null)
            instanceMethodList.SetActive(false);
        Color newCol;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
