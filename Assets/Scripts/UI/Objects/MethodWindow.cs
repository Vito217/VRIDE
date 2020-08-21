using System;
using PharoModule;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

public class MethodWindow : BrowserWindow
{
    public override async void Load()
    {
        string code = "", res = "";
        string className = theBrowser.class_list.getLastSelected().name;
        string[] methods = null;
        try
        {
            await Task.Run(async () => {
                code = theBrowser.classSideToggle.isOn ?
                    "(" + className + " class) methodDict keys asString ." :
                    className + " methodDict keys asString .";
                res = await Pharo.Execute(code);
                methods = Regex.Replace(res, @"'|\(|\)|#|\n", "").Split(' ');
                Array.Sort(methods, StringComparer.InvariantCulture);
            });

            foreach (Transform child in transform)
                if (child.gameObject.name != "template") 
                    Destroy(child.gameObject);

            if (methods.Length > 0)
                foreach (string method in methods) 
                    if (!string.IsNullOrWhiteSpace(method))
                        Instantiator.Instance.MethodObject(method, theBrowser);

            LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        }
        catch(Exception e)
        {
            theBrowser.field.text += " -> [Error] " + e.Message; 
        }
        theBrowser.Reactivate();
    }
}
