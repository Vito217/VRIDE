using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class IJavascript : MonoBehaviour
{
    IronJS.Hosting.CSharp.Context context;

    private void Start()
    {
        context = new IronJS.Hosting.CSharp.Context();
    }

    public string Execute(string code)
    {
        string result = context.Execute(code).ToString();
        return result;
    }
}
