using PharoModule;
using System;

public class BrowserMethod : BrowserObject
{
    public override async void onSelect()
    {
        theBrowser.field.text = sourceCode;

        if (name != "template")
        {
            string code = "";
            string aClass = theBrowser.class_list.getLastSelected().name;
            if (theBrowser.classSideToggle.isOn)
            {
                theBrowser.classSideList.gameObject.SetActive(true);
                theBrowser.instanceSideList.gameObject.SetActive(false);
                code = "((" + aClass + " class)>>#" + name + ") sourceCode .";
            }
            else
            {
                theBrowser.classSideList.gameObject.SetActive(false);
                theBrowser.instanceSideList.gameObject.SetActive(true);
                code = "(" + aClass + ">>#" + name + ") sourceCode .";
            }

            try
            {
                theBrowser.field.text = await Pharo.Execute(code);
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
    }
}
