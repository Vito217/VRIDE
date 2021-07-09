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
using LoggingModule;
using UnityEngine.EventSystems;
using SaveAndLoad;

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

    public TextMeshPro textPrefab;

    Dictionary<string, GameObject> importLines;
    Thread execution;
    IPython engine;

    Dictionary<string, Color> colorDict = new Dictionary<string, Color>()
    {
        { "def", Color.yellow },
        { "if", Color.yellow },
        { "elif", Color.yellow },
        { "else", Color.yellow },
        { "print", Color.cyan },
        { "import", Color.green },
        { "True", Color.blue },
        { "False", Color.blue },
        { "for", Color.yellow },
        { "in", Color.yellow },
        { "from", Color.green },
        { "None", Color.red },
        { "class", Color.green },
        { "break", Color.yellow },
        { "pass", Color.yellow }
    };

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
        MatchCollection imports = Regex.Matches(pythonCode.text, @"(from|import)\s+([a-zA-Z0-9]+)");
        foreach(Match import in imports)
        {
            string importedFile = import.Groups[2].Value + ".py";
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

                    TextMeshPro text = Instantiate(textPrefab, line.transform, false);
                    text.fontSize = 0.25f;
                }
                else
                {
                    line = importLines[importedFile].GetComponent<LineRenderer>();
                }
                
                line.SetPosition(0, transform.Find("Panel/Toolbar").position);
                line.SetPosition(1, editor.transform.Find("Panel/Toolbar").position);

                line.transform.GetChild(0).position = (line.GetPosition(0) + line.GetPosition(1)) / 2;
                line.transform.GetChild(0).GetComponent<TextMeshPro>().text = 
                    name.Replace(".py", "") + " imports " + importedFile.Replace(".py", "") + "\n";

                MatchCollection extends = Regex.Matches(pythonCode.text, @"class\s+([a-zA-Z0-9]+)\(\s*([a-zA-Z0-9.]+)\s*\)");
                foreach (Match extend in extends)
                {
                    string className = extend.Groups[1].Value;
                    string[] baseClassImport = extend.Groups[2].Value.Split('.');
                    string baseClass = baseClassImport[baseClassImport.Length - 1];

                    string importedCode = editor.GetComponent<PythonEditor>().pythonCode.text;

                    if (Regex.Match(importedCode, @"class\s*" + baseClass).Success)
                        line.transform.GetChild(0).GetComponent<TextMeshPro>().text +=
                            "class " + className + " extends " + baseClass + "\n";
                }
            }
        }
    }

    void LateUpdate()
    {
        CountLines();
        HighlightCode();
    }

    public override void onClose()
    {
        if ((execution != null && !execution.ThreadState.Equals(ThreadState.Running)) || execution == null)
        {
            foreach (GameObject import in importLines.Values)
                Destroy(import);

            InteractionLogger.Discount("PythonEditor", GetInstanceID().ToString());
            SaveAndLoadModule.pyEditors.Remove(this);

            base.onClose();
        }
    }

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("PythonEditor", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("PythonEditor", GetInstanceID().ToString());
    }

    public void Run()
    {
        HandleRun();
    }

    async void HandleRun()
    {
        Save();

        logText.text = "";
        runButton.interactable = false;
        stopButton.interactable = true;

        await Task.Run(() =>
        {
            engine.ResetStream();
            execution = new Thread(() => engine.ExecuteFile(fullpath));
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

        InteractionLogger.RegisterPythonExecution(fullpath, pythonCode.text, logText.text);
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

    void HighlightCode()
    {
        TMP_TextInfo textInfo = field.textComponent.textInfo;

        foreach (TMP_WordInfo wordInfo in textInfo.wordInfo)
            PaintWord(textInfo, wordInfo, Color.white);

        for (int i = 0; i < textInfo.wordInfo.Length; i++)
        {
            try
            {
                TMP_WordInfo wordInfo = textInfo.wordInfo[i];
                string word = wordInfo.GetWord();
                Color color = colorDict[word];
                PaintWord(textInfo, wordInfo, color);
            }
            catch
            {
                continue;
            }
        }
        field.textComponent.UpdateVertexData(TMP_VertexDataUpdateFlags.All);
    }
}
