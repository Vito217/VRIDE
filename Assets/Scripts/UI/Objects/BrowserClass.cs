using UnityEngine;
using PharoModule;
using System.Text.RegularExpressions;
using System;
using TMPro;

public class BrowserClass : BrowserObject
{
    public override async void onSelect()
    {
        theBrowser.DeactivateTemporarily();
        GetComponent<TextMeshProUGUI>().color = theBrowser.skyBlue;
        BrowserClass last = theBrowser.class_list.getLastSelected() as BrowserClass;
        if (last != null) last.onDeselect();
        theBrowser.class_list.setLastSelected(this);
        if (name != "template")
        {
            try
            {
                theBrowser.methodList.gameObject.SetActive(true);
                string sourceCode = await Pharo.Execute(name + " definition .");
                sourceCode = sourceCode.Substring(1, sourceCode.Length - 3);
                theBrowser.field.text = sourceCode;
                theBrowser.methodList.Load();
            }
            catch (Exception e)
            {
                theBrowser.field.text += " -> [Error] " + e.Message;
                theBrowser.Reactivate();
            }
        }
        else
        {
            theBrowser.methodList.gameObject.SetActive(false);
            theBrowser.field.text = 
                "Object subclass: #NameOfSubclass\n" +
                    "    instanceVariableNames: ''\n" +
                    "    classVariableNames: ''\n" +
                    "    package: 'MyPackage'";
            theBrowser.Reactivate();
        }
    }

    public override void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }
}
