using SaveAndLoad;
using System;
using System.IO;
using UnityEngine;

public class IPython : MonoBehaviour
{
    public MemoryStream stream;
    public VRIDEWriter writer;

    public MemoryStream errorStream;
    public VRIDEWriter errorWriter;

    public Microsoft.Scripting.Hosting.ScriptEngine pythonEngine;
    public Microsoft.Scripting.Hosting.ScriptScope pythonScope;

    void Start()
    {   
        pythonEngine = IronPython.Hosting.Python.CreateEngine();

        var paths = new[] {
            Path.Combine(Application.persistentDataPath),
            Path.Combine(Application.streamingAssetsPath),
            Path.Combine(Application.streamingAssetsPath, "Lib"),
            Path.Combine(Application.streamingAssetsPath, "Lib", "site-packages"),
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
        }
        catch (Exception e)
        {
            errorWriter.Write(e.StackTrace);
        }
    }

    void InitializeStream()
    {
        stream = new MemoryStream();
        writer = new VRIDEWriter(stream);

        errorStream = new MemoryStream();
        errorWriter = new VRIDEWriter(errorStream);

        pythonEngine.Runtime.IO.SetOutput(stream, writer);
        pythonEngine.Runtime.IO.SetErrorOutput(errorStream, errorWriter);
    }

    public void ResetStream()
    {
        if (stream != null) stream.Dispose();
        if (writer != null) writer.Dispose();
        if (errorStream != null) errorStream.Dispose();
        if (errorWriter != null) errorWriter.Dispose();
        InitializeStream();
    }
}
