using PharoModule;
using System;
using TMPro;

public class BrowserMethod : BrowserObject
{
    public override async void onSelect()
    {
        theBrowser.DeactivateTemporarily();
        GetComponent<TextMeshProUGUI>().color = theBrowser.skyBlue;
        BrowserMethod last = theBrowser.methodList.getLastSelected() as BrowserMethod;
        if (last != null) last.onDeselect();
        theBrowser.methodList.setLastSelected(this);
        if (name != "template")
        {
            string aClass = theBrowser.class_list.getLastSelected().name;
            string code = theBrowser.classSideToggle.isOn ?
                "((" + aClass + " class)>>#" + name + ") sourceCode ." :
                "(" + aClass + ">>#" + name + ") sourceCode .";
            try
            {
                string sourceCode = await Pharo.Execute(code);
                sourceCode = sourceCode.Substring(1, sourceCode.Length - 3);
                theBrowser.field.text = sourceCode;
            }
            catch (Exception e)
            {
                theBrowser.field.text += " -> [Error] " + e.Message;
            }
        }
        else
        {
            theBrowser.field.text =
                "messageSelectorAndArgumentNames\n" +
                    "    | temporary variable names |\n" +
                    "    statements";
        }
        theBrowser.Reactivate();
    }

    public override void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = theBrowser.white;
    }
}
