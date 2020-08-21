using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using SaveAndLoad;
using PharoModule;
using UnityEngine;
using UnityEngine.UI;

public class ClassWindow : BrowserWindow
{
    public async void Load()
    {
        theBrowser.DeactivateTemporarily();

        string package = "", code = "", res = "";
        string[] classes = null;

        await Task.Run(async () => {
            package = theBrowser.package_list.getLastSelected().name;
            code = "(RPackageOrganizer packageOrganizer packageNamed: '" + package + "') classes asString .";
            res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "");
            classes = res.Split(' ');
            Array.Sort(classes, StringComparer.InvariantCulture);
        });

        if (classes.Length > 0 && classes[0] != "")
        {
            foreach (string aClass in classes) if (aClass != "class")
                Instantiator.Instance.ClassObject(this, aClass, theBrowser.field,
                    null, null, "", theBrowser);
        }
        LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        theBrowser.Reactivate();
    }
}
