using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

public class InspectorInit : MonoBehaviour
{
    public GameObject inspector_content;
    public GameObject inspector_row_prefab;

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    public void initializeContent(string responseString) {
        responseString = Regex.Replace(responseString, @"an OrderedCollection\((.*)\)", "$1");
        responseString = Regex.Replace(responseString, @"self=a\s(.*)", "self=a$1");
        string[] tuples = responseString.Split(' ');
        foreach (string tuple in tuples)
        {
            string[] pair = tuple.Split('=');
            string variable = pair[0].Replace("'", "");
            string value = pair[1].Replace("'", "");

            GameObject new_row = Instantiate(inspector_row_prefab);
            TextMeshProUGUI var_button = new_row.transform.Find("Variable").transform.Find("Text (TMP)").GetComponent<TextMeshProUGUI>();
            TextMeshProUGUI val_button = new_row.transform.Find("Value").transform.Find("Text (TMP)").GetComponent<TextMeshProUGUI>();
            var_button.text = variable;
            val_button.text = value;
            new_row.transform.SetParent(inspector_content.transform, false);
            new_row.name = tuple;
        }
    }
}
