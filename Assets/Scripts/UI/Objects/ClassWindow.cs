using System;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;
using UnityEngine;
using UnityEngine.UI;

public class ClassWindow : BrowserWindow
{
    public override async void Load()
    {
        string package = "", code = "", res = "";
        string[] classes = null;
        try
        {
            package = theBrowser.package_list.getLastSelected().name;
            await Task.Run(async () => {
                code = "(RPackageOrganizer packageOrganizer packageNamed: '" + package + "') classes asString .";
                res = await Pharo.Execute(code);
                res = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "");
                classes = res.Split(' ');
                Array.Sort(classes, StringComparer.InvariantCulture);
            });

            foreach (Transform child in transform)
                if (child.gameObject.name != "template")
                    Destroy(child.gameObject);

            if (classes.Length > 0)
                foreach (string aClass in classes) 
                    if (aClass != "class" && !String.IsNullOrWhiteSpace(aClass))
                        Instantiator.Instance.ClassObject(aClass, theBrowser);

            LayoutRebuilder.ForceRebuildLayoutImmediate(GetComponent<RectTransform>());
        }
        catch (Exception e)
        {
            theBrowser.field.text += " -> [Error] " + e.Message;
        }
        theBrowser.Reactivate();
    }
}
