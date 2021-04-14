using System;
using System.Collections;
using System.Collections.Generic;
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
    public Scrollbar outputScrollBar;

    Dictionary<string, GameObject> importLines;
    Thread execution;
    IPython engine;

    public override IEnumerator innerStart()
    {
        importLines = new Dictionary<string, GameObject>();
        engine = GetComponent<IPython>();
        name = Path.GetFileName(fullpath);
        filename.text = name;
        yield return base.innerStart();
    }

    public override void innerBehaviour()
    {
        base.innerBehaviour();

        // Cleaning previous imports
        foreach (string import in importLines.Keys)
        {
            GameObject editor = GameObject.Find(import);
            if (!editor)
            {
                Destroy(importLines[import].gameObject);
                importLines.Remove(import);
            }
        }

        // Check for imports
        MatchCollection imports = Regex.Matches(pythonCode.text, @"import ([a-zA-Z0-9]+)");
        foreach(Match import in imports)
        {
            string importedFile = import.Groups[1].Value + ".py";
            GameObject editor = GameObject.Find(importedFile);

            if (editor)
            {
                LineRenderer line;

                if (!importLines.ContainsKey(importedFile))
                {
                    importLines.Add(importedFile, new GameObject("Line", typeof(LineRenderer)));
                    line = importLines[importedFile].GetComponent<LineRenderer>();
                    line.material = Instantiator.Instance.lineRendererMaterial;
                    line.startWidth = .002f;
                    line.endWidth = .002f;
                }
                else
                {
                    line = importLines[importedFile].GetComponent<LineRenderer>();
                }
                
                line.SetPosition(0, transform.Find("Panel/Toolbar").position);
                line.SetPosition(1, editor.transform.Find("Panel/Toolbar").position);
            }
        }
    }

    public override void onClose()
    {
        base.onClose();
        foreach (GameObject import in importLines.Values)
            Destroy(import);
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
            try 
            { 
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
