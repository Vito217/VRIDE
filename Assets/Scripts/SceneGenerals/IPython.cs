using SaveAndLoad;
using System;
using System.IO;
using UnityEngine;

public class IPython : MonoBehaviour
{
    public MemoryStream stream;
    public VRIDEWriter writer;

    public Microsoft.Scripting.Hosting.ScriptEngine pythonEngine;
    public Microsoft.Scripting.Hosting.ScriptScope pythonScope;

    void Start()
    {   
        pythonEngine = IronPython.Hosting.Python.CreateEngine();

        var paths = new[] {
            Path.Combine(Application.persistentDataPath),
            Path.Combine(Application.streamingAssetsPath),
            Path.Combine(Application.streamingAssetsPath, "Python", "Lib"),
            Path.Combine(Application.streamingAssetsPath, "Python", "Lib", "site-packages"),
            Path.Combine(Application.persistentDataPath, SaveAndLoadModule.username)
        };

        pythonEngine.SetSearchPaths(paths);

        pythonScope = pythonEngine.CreateScope();

        InitializeStream();
    }

    public void Execute(string code)
    {
        try 
        {
            pythonEngine.Execute(code, pythonScope);
            writer.Write("\n\nProgram executed successfully.");
        }
        catch (Exception e)
        {
            writer.Write(e.Message + "\n" + e.StackTrace);
            writer.Write("\n\nProgram failed execution.");
        }
    }

    public void ExecuteFile(string path)
    {
        try
        {
            //pythonEngine.ExecuteFile(path, pythonScope);

            Microsoft.Scripting.Hosting.ScriptSource pythonScript =
                pythonEngine.CreateScriptSourceFromFile(path);

            pythonScript.Execute(pythonScope);

            writer.Write("\n\nProgram executed successfully.");
        }
        catch (Exception e)
        {
            writer.Write(e.Message + "\n" + e.StackTrace);
            writer.Write("\n\nProgram failed execution.");
        }
    }

    void InitializeStream()
    {
        stream = new MemoryStream();
        writer = new VRIDEWriter(stream);

        pythonEngine.Runtime.IO.SetOutput(stream, writer);
        pythonEngine.Runtime.IO.SetErrorOutput(stream, writer);
    }

    public void ResetStream()
    {
        if (stream != null) stream.Dispose();
        if (writer != null) writer.Dispose();
        InitializeStream();
    }
}
