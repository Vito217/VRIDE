using RDotNet;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class R : MonoBehaviour
{
    public REngine rEngine;

    // Update is called once per frame
    public void Execute(string code)
    {
        //REngine.SetEnvironmentVariables();
        rEngine = REngine.GetInstance();
        //var result = rEngine.Evaluate(code);
        
        
        rEngine.Dispose();
    }
}
