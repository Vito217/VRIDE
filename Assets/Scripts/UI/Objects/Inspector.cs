using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;
using InstantiatorModule;
using LoggingModule;

public class Inspector : InitializeBehaviour
{
    public Transform inspector_content;
    public string data;

    public void setContent(string response) {
        data = response;
        response = response.Replace("an OrderedCollection('", "");
        response = response.Replace("')", "");
        foreach (string tuple in response.Split(new string[] { "' '" }, StringSplitOptions.None))
        {
            string[] pair = tuple.Replace("'", "").Split('=');
            InspectorRow new_row = Instantiator.InspectorDataRow();
            new_row.setContent(pair[0], pair[1], inspector_content);
        }
    }

    public override void onClose()
    {
        player.inspectors.Remove(this);
        InteractionLogger.Discount("Inspector");
        Destroy(gameObject);
    }
}
