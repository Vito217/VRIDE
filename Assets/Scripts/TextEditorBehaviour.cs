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
    public static readonly HttpClient client = new HttpClient();
    private string pattern2 = @"(\b)(void|string|int|char|float|double|bool|static|public|private|def|print|return|Object)(\s)";
    private List<string> keywords = new List<string> { 
        "void", "string", "int", "char", "float", "double", "bool", "static", "public", "private", "def", "print", "return"
    };

    public void onChangeInput()
    {
        string text = field.text;
        int last_caret_position = field.caretPosition;
        text = Regex.Replace(text, @"\s<color=#b32d00>(.*)", "");
        text = Regex.Replace(text, @"</color>", "");
        text = Regex.Replace(text, @"<color=#00ffffff>", "");

        if (Regex.Match(text, pattern2).Success)
        {
            if (Input.GetKeyDown("space"))
            {
                int aux_pos = last_caret_position;
                string last_char = text[aux_pos].ToString();
                StringBuilder sb = new StringBuilder();
                for(int i=aux_pos-1; i >= 0 && text[i].ToString() != " "; i--)
                {
                    sb.Insert(0, text[i].ToString(), 1);
                }
                string previous_word = sb.ToString();
                if (keywords.Contains(previous_word))
                {
                    last_caret_position += 1;
                }
            }
            text = Regex.Replace(text, @"(\b)(void|string|int|char|float|double|bool|static|public|private|def|print|return|Object)(\s)", "$1<color=#00ffffff>$2</color>$3");
            text = Regex.Replace(text, @"(\b)(#)(.*)(\s|\n|\t)", "$1<color=#00ffffff>$2$3</color>$4");
        }

        field.text = text;
        field.caretPosition = last_caret_position;
    }

}
