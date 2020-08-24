using PharoModule;
using System;

public class BrowserClass : BrowserObject
{
    public override async void onSelect()
    {
        base.onSelect();
        if (theBrowser.class_list.last_selected != null)
            theBrowser.class_list.last_selected.onDeselect();
        theBrowser.class_list.last_selected = this;
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
        }
    }
}
