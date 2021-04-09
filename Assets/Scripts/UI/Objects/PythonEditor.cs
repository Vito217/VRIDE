using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class PythonEditor : InitializeBehaviour
{
    [HideInInspector]
    public string fullpath;

    public Button saveButton;
    public Button runButton;
    public Button stopButton;
    public TMP_InputField pythonCode;
    public TextMeshProUGUI filename;

    Thread execution;
    IPython engine;

    public override IEnumerator innerStart()
    {
        engine = GetComponent<IPython>();
        filename.text = Regex.Match(fullpath, "([a-zA-Z0-9]+.py)").Value;
        StartCoroutine(LogOutput());
        yield return base.innerStart();
    }

    public override void innerBehaviour()
    {
        base.innerBehaviour();
    }

    public void Run()
    {
        HandleRun();
    }

    async void HandleRun()
    {
        runButton.interactable = false;
        stopButton.interactable = true;

        await Task.Run(() =>
        {
            engine.ResetStream();
        });

        execution = new Thread(() =>
        {
            engine.Execute(pythonCode.text);
        });

        await Task.Run(() =>
        {
            try
            {
                execution.Start();
                execution.Join();
            }
            catch { }
        });

        runButton.interactable = true;
        stopButton.interactable = false;
    }

    public void Stop()
    {
        execution.Abort();
    }

    public void Save()
    {
        File.WriteAllText(fullpath, keyboardTarget.text);
        saveButton.interactable = false;
    }

    public void OnFileChange()
    {
        saveButton.interactable = true;
    }

    IEnumerator LogOutput()
    {
        while (true)
        {
            logText.text += engine.writer.GetContentFromBuffer();
            yield return new WaitForSeconds(1);
        }
    }
}
