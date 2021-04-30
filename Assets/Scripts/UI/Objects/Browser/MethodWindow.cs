using System;
using System.Linq;
using PharoModule;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

public class MethodWindow : BrowserWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = theBrowser.classSideToggle.isOn ?
                "(" + key + " class) methodDict keys asString ." :
                key + " methodDict keys asString .";
            string res = await Pharo.Execute(code);
            contents = Regex.Replace(res, @"'|\(|\)|#|\n", "").Split(' ');
            if (!String.IsNullOrWhiteSpace(theBrowser.methodFilter.text))
                contents = contents.Where(s => s.StartsWith(theBrowser.methodFilter.text)).ToArray();
            Array.Sort(contents, StringComparer.InvariantCulture);
        });
    }

    public override void InnerFill(string content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            Instantiator.Instance.MethodObject(content, theBrowser);
    }

    public override string QueryKey()
    {
        return theBrowser.class_list.last_selected.name;
    }
}
