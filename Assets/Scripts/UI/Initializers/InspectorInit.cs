using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

public class InspectorInit : InitializeBehaviour
{
    public Transform inspector_content;
    public InspectorRow inspector_row_prefab;
    public Image table_panel;
    public Image editor_panel;
    public string data;

    // Start is called before the first frame update
    void Start()
    {
        Color new_color = UnityEngine.Random.ColorHSV();
        table_panel.color = new_color;
        editor_panel.color = new_color;
    }

    public void setContent(string response) {
        data = response;
        response = response.Replace("an OrderedCollection('", "");
        response = response.Replace("')", "");
        foreach (string tuple in response.Split(new string[] { "' '" }, StringSplitOptions.None))
        {
            string[] pair = tuple.Replace("'", "").Split('=');
            InspectorRow new_row = Instantiate(inspector_row_prefab);
            new_row.setContent(pair[0], pair[1], inspector_content);
        }
    }
}
