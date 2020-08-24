using System;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;

public class PackageWindow : BrowserWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = "RPackageOrganizer packageOrganizer packageNames .";
            string res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"a SortedCollection\(#|#\(#|'|\)|\n", "");
            contents = res.Split(new string[] { " #" }, StringSplitOptions.None);
            Array.Sort(contents, StringComparer.InvariantCulture);
        });
    }

    public override void InnerFill(string content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            Instantiator.Instance.PackageObject(content, theBrowser);
    }

    public override string QueryKey()
    {
        return null;
    }
}
