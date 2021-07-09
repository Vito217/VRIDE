using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;
using System.IO;
using LoggingModule;
using SaveAndLoad;
using UnityEngine.EventSystems;
using System.Threading.Tasks;
using System;

public class REditor : InitializeBehaviour
{
    [HideInInspector]
    public string fullpath;

    public Button saveButton;
    public Button compileButton;
    public TMP_InputField rCode;
    public TextMeshProUGUI filename;
    public Scrollbar outputScrollBar;

    public R rComponent;

    Dictionary<string, Color> colorDict = new Dictionary<string, Color>()
    {
        { "<-", Color.green },
        { "print", Color.green }
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
        InteractionLogger.Discount("REditor", GetInstanceID().ToString());

        base.onClose();
    }

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("REditor", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("REditor", GetInstanceID().ToString());
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
            await Task.Run(() =>
            {
                rComponent.Execute(rCode.text);
            });
        }
        catch(Exception e)
        {
            logText.text = e.Message;
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
