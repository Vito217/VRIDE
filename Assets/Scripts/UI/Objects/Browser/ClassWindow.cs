using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;

public class ClassWindow : BrowserWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = "(RPackageOrganizer packageOrganizer packageNamed: '" 
                + key + "') classes asString .";
            string res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "");
            contents = res.Split(' ');
            if (!String.IsNullOrWhiteSpace(theBrowser.classFilter.text))
                contents = contents.Where(s => s.StartsWith(theBrowser.classFilter.text)).ToArray();
            Array.Sort(contents, StringComparer.InvariantCulture);
        });
    }

    public override void InnerFill(string content)
    {
        if (content != "class" && !String.IsNullOrWhiteSpace(content))
            Instantiator.Instance.ClassObject(content, theBrowser);
    }

    public override string QueryKey()
    {
        return theBrowser.package_list.last_selected.name;
    }
}
