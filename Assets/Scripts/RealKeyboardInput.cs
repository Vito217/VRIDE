using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using TMPro;
using IronPython.Hosting;

public class RealKeyboardInput : MonoBehaviour
{
    public TextMeshProUGUI code;
    private bool key_down = false;
    private bool block_mayus = false;
    private int cursor_index = 0;
    private StringBuilder sb = new StringBuilder(" ");

    private Dictionary<string, string> dict = new Dictionary<string, string>();

    void Start()
    {
        code.text = "_";

        dict.Add("A", "A");
        dict.Add("B", "B");
        dict.Add("C", "C");
        dict.Add("D", "D");
        dict.Add("E", "E");
        dict.Add("F", "F");
        dict.Add("G", "G");
        dict.Add("H", "H");
        dict.Add("I", "I");
        dict.Add("J", "J");
        dict.Add("K", "K");
        dict.Add("L", "L");
        dict.Add("M", "M");
        dict.Add("N", "N");
        dict.Add("O", "O");
        dict.Add("P", "P");
        dict.Add("Q", "Q");
        dict.Add("R", "R");
        dict.Add("S", "S");
        dict.Add("T", "T");
        dict.Add("U", "U");
        dict.Add("V", "V");
        dict.Add("W", "W");
        dict.Add("X", "X");
        dict.Add("Y", "Y");
        dict.Add("Z", "Z");
        dict.Add("Space", " ");
        dict.Add("Return", "\n");
        dict.Add("Tab", "\t");
        dict.Add("Equals", "=");
        dict.Add("Alpha1", "1");
        dict.Add("Alpha2", "2");
        dict.Add("Alpha3", "3");
        dict.Add("Alpha4", "4");
        dict.Add("Alpha5", "5");
        dict.Add("Alpha6", "6");
        dict.Add("Alpha7", "7");
        dict.Add("Alpha8", "8");
        dict.Add("Alpha9", "9");
        dict.Add("Alpha0", "0");
        dict.Add("Exclaim", "!");
        dict.Add("DoubleQuote", "\"\"");
        dict.Add("Hash", "#");
        dict.Add("Dolar", "%");
        dict.Add("Percent", "%");
        dict.Add("Ampersand", "&");
        dict.Add("Quote", "'");
        dict.Add("LeftParen", "(");
        dict.Add("RightParen", ")");
        dict.Add("Asterisk", "*");
        dict.Add("Plus", "+");
        dict.Add("Comma", ",");
        dict.Add("Minus", "-");
        dict.Add("Period", ".");
        dict.Add("Slash", "/");
        dict.Add("Colon", ":");
        dict.Add("Semicolon", ";");
        dict.Add("Less", "<");
        dict.Add("Greater", ">");
        dict.Add("Question", "?");
        dict.Add("At", "@");
        dict.Add("LeftBracket", "[");
        dict.Add("Backslash", "\\");
        dict.Add("RightBracket", "]");
        dict.Add("Caret", "^");
        dict.Add("Underscore", "_");
        dict.Add("BackQuote", "`");
        dict.Add("LeftCurlyBracket", "{");
        dict.Add("RightCurlyBracket", "}");
        dict.Add("Pipe", "|");
        dict.Add("Tilde", "~");

        /**
        var var1 = 1;
        var var2 = 2;

        var engine = Python.CreateEngine();
        var scope = engine.CreateScope();

        engine.Execute(@"my_var = 200", scope);

        dynamic testFunction = scope.GetVariable("my_var");
        Debug.Log(testFunction);
        **/
    }


    void OnGUI()
    {
        Event e = Event.current;
        if (e.isKey && e.keyCode.ToString() != "None")
        {
            key_down = !key_down;
            if (key_down)
            {
                string value = e.keyCode.ToString();
                Debug.Log(value);
                if (value == "LeftArrow")
                {
                    if (cursor_index > 0)
                    {
                        cursor_index -= 1;
                    }

                }
                else if (value == "RightArrow")
                {
                    if (cursor_index < sb.Length - 1)
                    {
                        cursor_index += 1;
                    }
                }
                else if (value == "UpArrow")
                {

                }
                else if (value == "DownArrow")
                {

                }
                else if(value == "LeftShift")
                {

                }
                else if(value == "RightShift")
                {

                }
                else if(value == "LeftControl")
                {

                }
                else if(value == "RightControl")
                {

                }
                else if(value == "LeftAlt")
                {

                }
                else if(value == "RightAlt")
                {

                }
                else if (value == "CapsLock")
                {
                    block_mayus = !block_mayus;
                }
                else if (value == "Backspace")
                {
                    if (sb.Length > 1 && cursor_index > 0)
                    {
                        cursor_index -= 1;
                        sb.Remove(cursor_index, 1);
                    }
                }
                else if (value == "Delete")
                {
                    if (sb.Length > 1 && cursor_index < sb.Length - 1)
                    {
                        sb.Remove(cursor_index, 1);
                    }
                }
                else
                {
                    value = dict[value];
                    value = block_mayus ? value : value.ToLower();
                    sb.Insert(cursor_index, value);
                    cursor_index += 1;
                }
                string result = sb.ToString();
                StringBuilder auxsb = new StringBuilder(result);

                char cursor_char = auxsb[cursor_index];
                auxsb.Remove(cursor_index, 1);
                if (cursor_char.ToString() == " " || cursor_char.ToString() == "\t" || cursor_char.ToString() == "\n")
                {
                    auxsb.Insert(cursor_index, "_");
                }
                else
                {
                    auxsb.Insert(cursor_index, "<u>" + cursor_char + "</u>");
                }

                result = auxsb.ToString();
                result = Regex.Replace(result, @"\b(void)\b", "<color=#00ffffff>void</color>");
                result = Regex.Replace(result, @"\b(string)\b", "<color=#00ffffff>string</color>");
                result = Regex.Replace(result, @"\b(int)\b", "<color=#00ffffff>int</color>");
                result = Regex.Replace(result, @"\b(char)\b", "<color=#00ffffff>char</color>");
                result = Regex.Replace(result, @"\b(float)\b", "<color=#00ffffff>float</color>");
                result = Regex.Replace(result, @"\b(double)\b", "<color=#00ffffff>double</color>");
                result = Regex.Replace(result, @"\b(bool)\b", "<color=#00ffffff>bool</color>");
                result = Regex.Replace(result, @"\b(static)\b", "<color=#00ffffff>static</color>");
                result = Regex.Replace(result, @"\b(public)\b", "<color=#00ffffff>public</color>");
                result = Regex.Replace(result, @"\b(private)\b", "<color=#00ffffff>private</color>");
                result = Regex.Replace(result, @"\b(def)\b", "<color=#00ffffff>def</color>");
                result = Regex.Replace(result, @"\b(print)\b", "<color=#00ffffff>print</color>");
                code.text = result;
            }
        }
    }
}
