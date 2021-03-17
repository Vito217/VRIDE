using PharoModule;
using System;

public class BrowserMethod : BrowserObject
{
    public override async void onSelect()
    {
        theBrowser.DeactivateTemporarily();
        theBrowser.logText.text = "";
        if (theBrowser.methodList.last_selected != null)
            theBrowser.methodList.last_selected.onDeselect();
        theBrowser.methodList.last_selected = this;
        if (name != "template")
        {
            string aClass = theBrowser.class_list.last_selected.name;
            string code = theBrowser.classSideToggle.isOn ?
                "((" + aClass + " class)>>#" + name + ") sourceCode ." :
                "(" + aClass + ">>#" + name + ") sourceCode .";
            try
            {
                string sourceCode = await Pharo.Execute(code);
                sourceCode = sourceCode.Substring(1, sourceCode.Length - 3);
                theBrowser.field.text = sourceCode;
                theBrowser.methodRemover.interactable = true;
            }
            catch (Exception e)
            {
                theBrowser.logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
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
        base.onSelect();
    }
}
