using System;
using System.Collections;
using SaveAndLoad;
using PharoModule;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using System.Collections.Generic;

public class MethodWindow : BrowserWindow
{
    public string package;
    public string side;

    public async void Load()
    {
        theBrowser.DeactivateTemporarily();
        string code = "", res = "";
        string className = theBrowser.class_list.getLastSelected().name;
        string[] methods = null;

        await Task.Run(async () => {
            code = side == "ClassSide" ?
                "(" + className + " class) methodDict keys asString ." :
                className + " methodDict keys asString .";
            res = await Pharo.Execute(code);
            methods = Regex.Replace(res, @"'|\(|\)|#|\n", "").Split(' ');
            Array.Sort(methods, StringComparer.InvariantCulture);
        });
        foreach(string method in methods)
        {
            Instantiator.Instance.MethodObject(transform, className, method,
                    theBrowser.field, "", theBrowser);
        }
        LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        theBrowser.Reactivate();
    }
}
