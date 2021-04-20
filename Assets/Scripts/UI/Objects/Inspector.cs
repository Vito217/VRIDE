using System;
using UnityEngine;
using UnityEngine.UI;
using LoggingModule;
using SaveAndLoad;

public class Inspector : InitializeBehaviour
{
    public Transform inspector_content;
    public RectTransform inspectorText;
    public string data;

    public void setContent(string response) {

        // Reseting
        foreach (Transform child in inspector_content) { 
            Destroy(child.gameObject); 
        }

        // Filling
        data = response;
        response = response.Replace("an OrderedCollection('", "");
        response = response.Replace("')", "");
        string text = "";
        foreach (string tuple in response.Split(new string[] { "' '" }, StringSplitOptions.None))
        {
            string[] pair = tuple.Replace("'", "").Split('=');
            InspectorRow new_row = Instantiator.Instance.InspectorDataRow();
            new_row.setContent(pair[0], pair[1], inspector_content, this);
            if (pair[0] == "self")
                text = pair[1] + "\n" + pair[0] + "\n";
        }
        field.text = text;
        LayoutRebuilder.ForceRebuildLayoutImmediate(inspectorText);
    }

    public override void onClose()
    {
        if (loadingWheel == null || !loadingWheel.activeSelf)
        {
            SaveAndLoadModule.inspectors.Remove(this);
            InteractionLogger.Discount("Inspector", GetInstanceID().ToString());
            Destroy(gameObject);
        }
    }
}
