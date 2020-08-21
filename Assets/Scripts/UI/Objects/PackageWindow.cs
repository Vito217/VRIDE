using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;
using UnityEngine;
using UnityEngine.UI;

public class PackageWindow : BrowserWindow
{
    void Start()
    {
        Load();
    }

    async void Load()
    {
        theBrowser.DeactivateTemporarily();
        string code = "", res = "";
        string[] packages = null;

        await Task.Run(async () => {
            code = "RPackageOrganizer packageOrganizer packageNames .";
            res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"#\(#|'|\)|\n", "");
            packages = res.Split(new string[] { " #" }, StringSplitOptions.None);
            Array.Sort(packages, StringComparer.InvariantCulture); 
        });
        foreach (string package in packages)
            Instantiator.Instance.PackageObject(this, package, null, theBrowser);
        LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        theBrowser.Reactivate();
    }
}
