using PharoModule;
using System;

public class BrowserMethod : BrowserObject
{
    public override async void onSelect()
    {
        base.onSelect();
        theBrowser.DeactivateTemporarily();
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
}
