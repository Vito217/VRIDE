using System;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using PharoModule;

public class RMethodsWindow : RWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = key + " methodDict keys asString .";
            string res = await Pharo.Execute(code);
            contents = Regex.Replace(res, @"'|\(|\)|#|\n", "").Split(' ');
            Array.Sort(contents, StringComparer.InvariantCulture);
        });
    }

    public override void InnerFill(string content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            Instantiator.Instance.RoassalMethodObject(content, roassal);
    }

    public override string QueryKey()
    {
        return roassal.class_list.last_selected.name;
    }
}
