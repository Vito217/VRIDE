using System;
using System.Text.RegularExpressions;
using PharoModule;
using LoggingModule;

public class RoassalMethod : RoassalObject
{
    public override async void onSelect()
    {
        base.onSelect();
        roassal.DeactivateTemporarily();
        roassal.logText.text = "";
        if (roassal.methodList.last_selected != null)
            roassal.methodList.last_selected.onDeselect();
        roassal.methodList.last_selected = this;

        // Load a visualization
        string aClass = roassal.class_list.last_selected.name;
        string code = "(" + aClass + ">>#" + name + ") sourceCode .";
        try
        {
            string sourceCode = await Pharo.Execute(code);
            sourceCode = sourceCode.Substring(1, sourceCode.Length - 3);
            sourceCode = Regex.Replace(
                sourceCode, @"\A([a-zA-Z0-9\n\s\t<>:']*)(\|.*\|)", "$2");

            Playground playground = Instantiator.Instance.Playground();
            playground.transform.position = roassal.transform.position;
            playground.transform.forward = roassal.transform.forward;
            playground.field.text = sourceCode;

            InteractionLogger.RegisterRoassalExample(name);

            Destroy(roassal.gameObject);
        }
        catch (Exception e)
        {
            roassal.logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
            roassal.Reactivate();
        }
    }
}
