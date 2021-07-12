using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class IJavascript
{
    public static IronJS.Hosting.CSharp.Context context = new IronJS.Hosting.CSharp.Context();

    public static string Execute(string code)
    {
        string result = context.Execute(code).ToString();
        return result;
    }
}
