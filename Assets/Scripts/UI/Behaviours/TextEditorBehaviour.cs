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
    public string IP = "http://localhost:1701/repl";
    public static readonly HttpClient client = new HttpClient();
    private StringBuilder sb = new StringBuilder();
    private List<string> notAN = new List<string> { " ", "\n", "\t", "\r" };

    //private string pattern2 = @"(\b)(void|string|int|char|float|double|bool|static|public|private|def|print|return|Object)(\s|\n)";
    //private List<string> keywords = new List<string> {
    //    "void", "string", "int", "char", "float", "double", "bool", "static", "public", "private", "def", "print", "return"
    //};

    public void onChangeInput()
    {
        string text = field.text;
        int last_caret_position = field.caretPosition;
        text = Regex.Replace(text, @"<color=#b32d00>((.*)\n)*</color>", "");
        text = Regex.Replace(text, @"</color>", "");
        text = Regex.Replace(text, @"<color=#00ffffff>", "");
        text = Regex.Replace(text, @"<b>", "");
        text = Regex.Replace(text, @"</b>", "");

        //if (Regex.Match(text, pattern2).Success)
        //{
        if (Input.GetKeyDown(KeyCode.Space) || Input.GetKeyDown(KeyCode.Return))
        {
            int aux_pos = last_caret_position;
            string last_char = text[aux_pos].ToString();
            int i;
            for (i = aux_pos - 1; i >= 0 && !notAN.Contains(text[i].ToString()); i--)
            {
                sb.Insert(0, text[i].ToString(), 1);
            }
            string previous_word = sb.ToString();
            sb.Clear();
            int pw_len = previous_word.Length;
            int aux = i;
            for (i=aux; i>=0 && notAN.Contains(text[i].ToString()); i--) { }
            if ((i==-1) || (i>=0 && text[i].ToString()==".") || (pw_len > 0 && previous_word[0].ToString()=="#"))
            {
                last_caret_position += 1;
            }
        }
        //text = Regex.Replace(text, @"(\b)(void|string|int|char|float|double|bool|static|public|private|def|print|return|Object)(\s|\n)", "$1<color=#00ffffff>$2</color>$3");
        text = Regex.Replace(text, @"(\A|\.\s*\n*\s*)([a-zA-Z0-9]+)(\s|\n)", "$1<b>$2</b>$3");
        text = Regex.Replace(text, @"(\s|\n|\t)(#)([a-zA-Z0-9]+)(\s|\n|\t)", "$1<color=#00ffffff>$2$3</color>$4");
        //}

        field.text = text;
        field.caretPosition = last_caret_position;
    }

}