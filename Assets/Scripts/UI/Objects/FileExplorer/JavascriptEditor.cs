using LoggingModule;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class JavascriptEditor : InitializeBehaviour
{
    [HideInInspector]
    public string fullpath;

    public Button saveButton;
    public Button compileButton;
    public TMP_InputField jsCode;
    public TextMeshProUGUI filename;
    public Scrollbar outputScrollBar;

    Dictionary<string, Color> colorDict = new Dictionary<string, Color>()
    {
        { "break", Color.magenta },
        { "case", Color.magenta } ,
        { "catch", Color.magenta} ,
        { "continue", Color.magenta},
        { "debugger", Color.magenta},
        { "default", Color.magenta},
        { "delete", Color.magenta},
        { "do",Color.magenta } ,
        { "else",  Color.magenta},
        { "finally",Color.magenta },
        { "for", Color.magenta},
        { "function" , Color.magenta},
        { "if", Color.magenta},
        { "in",Color.magenta },
        { "instanceof" , Color.magenta},
        { "new", Color.magenta},
        { "return", Color.magenta},
        { "switch" , Color.magenta},
        { "this" , Color.magenta},
        { "throw" , Color.magenta},
        { "try" , Color.magenta},
        { "typeof" , Color.magenta},
        { "var" , Color.magenta},
        { "void", Color.magenta},
        { "while", Color.magenta},
        { "and", Color.magenta},
        { "with" , Color.magenta}
    };

    public override IEnumerator innerStart()
    {
        name = Path.GetFileName(fullpath);
        filename.text = name;
        yield return base.innerStart();
    }

    void LateUpdate()
    {
        CountLines();
        HighlightCode();
    }

    public override void onClose()
    {
        InteractionLogger.Discount("JSEditor", GetInstanceID().ToString());

        base.onClose();
    }

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("JSEditor", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("JSEditor", GetInstanceID().ToString());
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

    public void Run()
    {
        HandleRun();
    }

    async void HandleRun()
    {
        Save();

        logText.text = "";
        compileButton.interactable = false;

        try
        {
            string result = "";

            await Task.Run(() =>
            {
                result = IJavascript.Execute(jsCode.text);
                
            });

            logText.text = result;
        }
        catch (Exception e)
        {
            logText.text = e.Message + "\n" + e.StackTrace;
        }
        finally
        {
            compileButton.interactable = true;
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
