using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

public class TextEditorBehaviour : MonoBehaviour
{
    public TextMeshProUGUI code;
    public TMP_InputField field;
    public GameObject player;
    private StringBuilder sb = new StringBuilder();
    private List<char> notAN = new List<char> {' ','\n','\t','\r'};

    public void onChangeInput()
    {
        int last_caret_position = field.caretPosition;
        string text = field.text;
        text = Regex.Replace(text, @"<color=#b32d00>|<color=#00ffffff>|</color>|<b>|</b>", "");
        text = Regex.Replace(text, @"\t", "".PadRight(4));

        bool tab_pressed = Input.GetKeyDown(KeyCode.Tab);
        if (Input.GetKeyDown(KeyCode.Space) || Input.GetKeyDown(KeyCode.Return) || tab_pressed)
        {
            int i;
            for (i = last_caret_position - 1; i >= 0 && !notAN.Contains(text[i]); i--)
                sb.Insert(0, text[i].ToString(), 1);
            string previous_word = sb.ToString();
            int pw_len = previous_word.Length;
            for (i=i; i>=0 && notAN.Contains(text[i]); i--) { }
            if ((i==-1) || (i>=0 && text[i]=='.') || (pw_len > 0 && previous_word[0]=='#'))
                last_caret_position += 1;
            if (tab_pressed)
            {
                last_caret_position += 4;
                if (text[last_caret_position - 1] != ' ')
                    last_caret_position -= 1;
            }
            sb.Clear();
        }

        text = Regex.Replace(text, @"(\A|\.\s*\n*\s*)([a-zA-Z0-9]+)(\s|\n)", "$1<b>$2</b>$3");
        text = Regex.Replace(text, @"(\s|\n|\t)(#)([a-zA-Z0-9]+)(\s|\n|\t)", "$1<color=#00ffffff>$2$3</color>$4");

        field.text = text;
        field.caretPosition = last_caret_position;
    }

    public string cleanCode(string code)
    {
        return Regex.Replace(code, @"<color=#b32d00>|<color=#00ffffff>|</color>|<b>|</b>", "");
    }

    public void onSelect()
    {
        player.GetComponent<VRIDEController>().can_move = false;
    }

    public void onDeselect()
    {
        player.GetComponent<VRIDEController>().can_move = true;
    }
}