using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

public class PlaygroundTextEditor : TextEditorBehaviour
{
    public GameObject inspector_row_prefab;
    public GameObject inspector_content;

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetMouseButton(0) || Input.GetMouseButton(1)))
            {
                onChangeInput();
            }
            else if (Input.GetKey(KeyCode.LeftControl) && Input.GetKey("g"))
            {
                PharoPrint();
            }
            else if (Input.GetKey(KeyCode.LeftControl) && Input.GetKey("h"))
            {
                PharoInspect();
            }
            else if (Input.GetKey(KeyCode.LeftControl) && Input.GetKey("v"))
            {
                string text = field.text;
                text = Regex.Replace(text, @"<color=#b32d00>(.*)", "$1");
                field.text = text;
                onChangeInput();
            }
        }
    }

    async void PharoPrint()
    {
        string clean_code = field.text;
        clean_code = Regex.Replace(clean_code, @"<color=#b32d00>.*</color>", "");
        clean_code = clean_code.Replace("<color=#b32d00>", "");
        clean_code = clean_code.Replace("<color=#00ffffff>", "");
        clean_code = clean_code.Replace("</color>", "");
        clean_code = clean_code.Replace("<b>", "");
        clean_code = clean_code.Replace("</b>", "");
        clean_code = clean_code.Replace("<br>", " ");

        var content = new StringContent(clean_code, Encoding.UTF8);
        var response = await client.PostAsync("http://localhost:1701/repl", content);
        var responseString = await response.Content.ReadAsStringAsync();
        field.text += " <color=#b32d00>" + responseString + "</color>";
    }


    async void PharoInspect()
    {
        string clean_code = field.text;
        clean_code = Regex.Replace(clean_code, @"<color=#b32d00>.*</color>", "");
        clean_code = clean_code.Replace("<color=#b32d00>", "");
        clean_code = clean_code.Replace("<color=#00ffffff>", "");
        clean_code = clean_code.Replace("</color>", "");
        clean_code = clean_code.Replace("<b>", "");
        clean_code = clean_code.Replace("</b>", "");
        clean_code = clean_code.Replace("<br>", " ");

        // Getting selected text
        int start = field.selectionAnchorPosition;
        int end = field.caretPosition;
        if (end < start)
        {
            int aux = end;
            end = start;
            start = aux;
        }
        string text = field.text;
        int selection_length = end - start;
        string selection = text.Substring(start, selection_length);

        // Assuming selection is the variable to inspect,
        // we send the message
        string code = clean_code + "\n" +
            "tuples := OrderedCollection new.\n" +
            "tuples addLast: 'self=',(" + selection + " value asString).\n" +
            selection + " class instVarNames do: [ :each |\n" +
                "\ttuples addLast: (each asString),'=', ((" + selection + " instVarNamed: each value) asString).\n" +
            "].\n" +
            "tuples .";

        var content = new StringContent(code, Encoding.UTF8);
        var response = await client.PostAsync(IP, content);
        var responseString = await response.Content.ReadAsStringAsync();
        responseString = Regex.Replace(responseString, @"an OrderedCollection\((.*)\)", "$1");

        // Deleting previous children
        var children = new List<GameObject>();
        foreach (Transform child in inspector_content.transform) {
            children.Add(child.gameObject);
        }
        children.ForEach(child => Destroy(child));

        // Adding new children
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
