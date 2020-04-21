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
                PharoPrint(field.text);
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

    async void PharoPrint(string input_code)
    {
        var content = new StringContent(input_code, Encoding.UTF8);
        var response = await client.PostAsync("http://localhost:1701/repl", content);
        var responseString = await response.Content.ReadAsStringAsync();
        field.text += " <color=#b32d00>" + responseString + "</color>";
    }
}
