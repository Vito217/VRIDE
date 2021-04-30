using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;

public class RClassesWindow : RWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = "RPackageOrganizer packageOrganizer packageNames .";
            string res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"a SortedCollection\(#|#\(#|'|\)|\n", "");
            string[] packages = res.Split(new string[] { " #" }, StringSplitOptions.None);
            List<string> classList = new List<string>();
            foreach(string package in packages)
            {
                if(Regex.Match(package, @"Roassal2|Roassal3-Examples").Success)
                {
                    code = "(RPackageOrganizer packageOrganizer packageNamed: '"
                            + package + "') classes asString .";
                    res = await Pharo.Execute(code);
                    res = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "");
                    string[] classes = res.Split(' ');
                    foreach(string aClass in classes)
                        if (Regex.Match(aClass, @"RT.*Example|RS.*Examples").Success &&
                            !aClass.Contains("Abstract"))
                            classList.Add(aClass);
                }
            }
            contents = classList.ToArray();
            Array.Sort(contents, StringComparer.InvariantCulture);
        });
    }

    public override void InnerFill(string content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            Instantiator.Instance.RoassalClassObject(content, roassal);
    }

    public override string QueryKey()
    {
        return null;
    }
}
