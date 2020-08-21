using UnityEngine;
using PharoModule;
using System;
using TMPro;

public class BrowserClass : BrowserObject
{
    public override async void onSelect()
    {
        BrowserClass last_class = 
            theBrowser.class_list.getLastSelected() as BrowserClass;
        if (last_class != null) last_class.onDeselect();

        if (name != "template")
        {
            if (theBrowser.classSideToggle.isOn)
            {
                theBrowser.classSideList.gameObject.SetActive(true);
                theBrowser.instanceSideList.gameObject.SetActive(false);
            }
            else
            {
                theBrowser.classSideList.gameObject.SetActive(false);
                theBrowser.instanceSideList.gameObject.SetActive(true);
            }

            try
            {
                theBrowser.field.text = await Pharo.Execute(name + " definition .");
                theBrowser.class_list.setLastSelected(this);
                theBrowser.classSideList.Load();
                theBrowser.instanceSideList.Load();
            }
            catch (Exception e)
            {
                theBrowser.field.text += " -> [Error] " + e.Message;
            }
        }
        else
        {
            theBrowser.classSideList.gameObject.SetActive(false);
            theBrowser.instanceSideList.gameObject.SetActive(false);
            theBrowser.field.text = 
                "Object subclass: #NameOfSubclass\n" +
                    "    instanceVariableNames: ''\n" +
                    "    classVariableNames: ''\n" +
                    "    package: 'MyPackage'";
        }

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#00FFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }

    public override void onDeselect()
    {
        foreach(Transform child in theBrowser.classSideList.transform)
            if (child.gameObject.name != "template") 
                Destroy(child.gameObject);

        foreach (Transform child in theBrowser.instanceSideList.transform)
            if (child.gameObject.name != "template")
                Destroy(child.gameObject);

        Color newCol;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out newCol))
            GetComponent<TextMeshProUGUI>().color = newCol;
    }
}
