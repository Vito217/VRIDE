using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;
using UnityEngine;
using UnityEngine.UI;

public class PackageWindow : BrowserWindow
{
    public override async void Load()
    {
        string code = "", res = "";
        string[] packages = null;
        try
        {
            await Task.Run(async () => {
                code = "RPackageOrganizer packageOrganizer packageNames .";
                res = await Pharo.Execute(code);
                res = Regex.Replace(res, @"#\(#|'|\)|\n", "");
                packages = res.Split(new string[] { " #" }, StringSplitOptions.None);
                Array.Sort(packages, StringComparer.InvariantCulture);
            });

            foreach (Transform child in transform)
                if (child.gameObject.name != "template")
                    Destroy(child.gameObject);

            if (packages.Length > 0)
                foreach (string package in packages)
                    if(!string.IsNullOrWhiteSpace(package))
                        Instantiator.Instance.PackageObject(package, theBrowser);

            LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        }
        catch (Exception e)
        {
            theBrowser.field.text += " -> [Error] " + e.Message;
        }
        theBrowser.Reactivate();
    }
}
