using System.IO;
using UnityEngine;

public class IPython : MonoBehaviour
{
    public MemoryStream stream;
    public VRIDEWriter writer;
    public Microsoft.Scripting.Hosting.ScriptEngine pythonEngine;
    public Microsoft.Scripting.Hosting.ScriptScope pythonScope;

    void Awake()
    {   
        pythonEngine = IronPython.Hosting.Python.CreateEngine();
        pythonScope = pythonEngine.CreateScope();
        InitializeStream();
    }

    public void Execute(string code)
    {
        pythonEngine.Execute(code, pythonScope);
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
