using RDotNet;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class R : MonoBehaviour
{
    public REngine rEngine;

    private void Start()
    {
        string dllPath = Path.Combine(Application.streamingAssetsPath, "R", "bin", "x64", "R.dll");
        string rPath = Path.Combine(Application.streamingAssetsPath, "R", "bin", "x64");
        string rHome = Path.Combine(Application.streamingAssetsPath, "R");

        REngine.SetEnvironmentVariables(rPath, rHome);
        rEngine = REngine.GetInstance();
    }

    // Update is called once per frame
    public void Execute(string code)
    {
        var result = rEngine.Evaluate(code);
    }

    private void OnDestroy()
    {
        rEngine.Dispose();
    }
}
