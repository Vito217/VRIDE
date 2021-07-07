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

    public override IEnumerator innerStart()
    {
        name = Path.GetFileName(fullpath);
        filename.text = name;
        yield return base.innerStart();
    }

    void LateUpdate()
    {
        CountLines();
        //HighlightCode();
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

        Debug.Log(rCode.text);

        await Task.Run(() =>
        {
            rComponent.Execute(rCode.text);
        });

        compileButton.interactable = true;
    }
}
