using System.IO;
using System.Text;
using UnityEngine;
using UnityEngine.UI;
using TMPro;
using System.Collections;
using System.Threading;

public class IPython : MonoBehaviour
{
    public MemoryStream stream;
    public Microsoft.Scripting.Hosting.ScriptEngine pythonEngine;
    public Microsoft.Scripting.Hosting.ScriptScope pythonScope;

    public Button saveButton;
    public Button runButton;
    public Button stopButton;
    public TMP_InputField outputText;
    public TMP_InputField pythonCode;

    Thread execution;

    void Awake()
    {
        stream = new MemoryStream();
        pythonEngine = IronPython.Hosting.Python.CreateEngine();
        pythonScope = pythonEngine.CreateScope();
        pythonEngine.Runtime.IO.SetOutput(stream, Encoding.UTF8);
        pythonEngine.Runtime.IO.SetErrorOutput(stream, Encoding.UTF8);
    }

    public void Execute(string code)
    {
        pythonEngine.Execute(code, pythonScope);
    }

    public void Run()
    {
        try
        {
            outputText.text = "";
            runButton.interactable = false;
            stopButton.interactable = true;

            execution = new Thread(() => Execute(pythonCode.text));
            execution.Start();
            StartCoroutine(LogOutputText());
            execution.Join();
        }
        catch
        {

        }
        finally
        {
            runButton.interactable = true;
            stopButton.interactable = false;
        }
    }

    public void Stop()
    {
        execution.Abort();
    }

    public void Save()
    {
        string path = GetComponent<PythonEditor>().fullpath;
        string code = GetComponent<PythonEditor>().keyboardTarget.text;

        File.WriteAllText(path, code);

        saveButton.interactable = false;
    }

    IEnumerator LogOutputText()
    {
        if (outputText != null)
            while (execution.ThreadState.Equals(ThreadState.Running))
            {
                outputText.text = Encoding.UTF8.GetString(stream.ToArray());
                yield return null;
            }
        yield return null;
    }

    public void OnFileChange()
    {
        saveButton.interactable = true;
    }
}
