using System;
using System.Collections;
using UnityEngine;
using LoggingModule;

public class Inspector : InitializeBehaviour
{
    public Transform inspector_content;
    public string data;

    void Update()
    {
        if (initializing)
            initializeAnimation();
        else if (dragging)
            dragAction();
    }

    public void setContent(string response) {

        // Reseting
        field.text = "";
        foreach (Transform child in inspector_content) { 
            Destroy(child.gameObject); 
        }

        // Filling
        data = response;
        response = response.Replace("an OrderedCollection('", "");
        response = response.Replace("')", "");
        foreach (string tuple in response.Split(new string[] { "' '" }, StringSplitOptions.None))
        {
            string[] pair = tuple.Replace("'", "").Split('=');
            InspectorRow new_row = Instantiator.Instance.InspectorDataRow();
            new_row.setContent(pair[0], pair[1], inspector_content, this);
            if (pair[0] == "self")
                field.text = "\"" + pair[1].Replace("\n", "") + "\"\n" + pair[0];
        }
    }

    public override void onClose()
    {
        player.inspectors.Remove(this);
        InteractionLogger.Discount("Inspector");
        Destroy(gameObject);
    }

    public override IEnumerator Coroutine()
    {
        paintPanels();
        yield return null;
    }
}
