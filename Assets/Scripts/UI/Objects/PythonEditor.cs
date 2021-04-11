using System;
using System.Collections;
using System.IO;
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
    public Scrollbar outputScrollBar;

    Thread execution;
    IPython engine;

    public override IEnumerator innerStart()
    {
        engine = GetComponent<IPython>();
        name = Path.GetFileName(fullpath);
        filename.text = name;
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
        logText.text = "";
        runButton.interactable = false;
        stopButton.interactable = true;

        await Task.Run(() =>
        {
            engine.ResetStream();
            execution = new Thread(() => engine.Execute(pythonCode.text));
            execution.Start();
        });

        StartCoroutine(LogOutput());

        await Task.Run(() =>
        {
            try { execution.Join(); }
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
        while (engine.writer.buffer.Count == 0)
            yield return null;

        bool empty = false;
        while (!empty)
        {
            try 
            {
                logText.text += engine.writer.GetContentFromBuffer();
                outputScrollBar.value = 1f;
            }
            catch (InvalidOperationException)
            {
                empty = true;
            }
            yield return null;
        }
    }
}
