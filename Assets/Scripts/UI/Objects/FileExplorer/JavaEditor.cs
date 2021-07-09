using LoggingModule;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class JavaEditor : InitializeBehaviour
{
    [HideInInspector]
    public string fullpath;

    public Button saveButton;
    public Button compileButton;
    public TMP_InputField javaCode;
    public TextMeshProUGUI filename;
    public Scrollbar outputScrollBar;

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
        InteractionLogger.Discount("JavaEditor", GetInstanceID().ToString());

        base.onClose();
    }

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("JavaEditor", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("JavaEditor", GetInstanceID().ToString());
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
}
