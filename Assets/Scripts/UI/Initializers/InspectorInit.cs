﻿using System.Collections;
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

    // Start is called before the first frame update
    void Start()
    {
        Color new_color = Random.ColorHSV();
        table_panel.color = new_color;
        editor_panel.color = new_color;
    }

    public void setContent(string response) {
        response = Regex.Replace(response, @"an OrderedCollection\((.*)\)", "$1");
        response = Regex.Replace(response, @"self=a\s(.*)", "self=a$1");
        foreach (string tuple in response.Split(' '))
        {
            string[] pair = tuple.Replace("'", "").Split('=');
            InspectorRow new_row = Instantiate(inspector_row_prefab);
            new_row.setContent(pair[0], pair[1], inspector_content);
        }
    }
}
