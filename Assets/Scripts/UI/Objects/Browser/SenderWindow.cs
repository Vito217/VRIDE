using PharoModule;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using UnityEngine;

public class SenderWindow : BrowserWindow
{
    public override async Task InnerQuery(string key)
    {
        await Task.Run(async () => {
            string code = "SystemNavigation default allReferencesTo: " + key + " binding .";
            string res = await Pharo.Execute(code);
            res = Regex.Replace(res, @"an OrderedCollection\(|\)|\n", "");
            contents = res.Split(' ');
        });
    }

    public override void InnerFill(string content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            Instantiator.Instance.SenderObject(content, theBrowser);
    }

    public override string QueryKey()
    {
        return theBrowser.class_list.last_selected.name;
    }
}
