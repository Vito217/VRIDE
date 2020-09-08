using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Shift : VRKey
{
    public List<Letter> letters;

    public void OnClick()
    {
        int lcp = 0;
        if (keyboard.window != null)
        {
            lcp = keyboard.window.lastCaretPosition;
            keyboard.window.field.ActivateInputField();
        }
        name = name == "shift" ? "SHIFT" : "shift";
        visibleText.text = name;
        if (name == "SHIFT")
        {
            foreach(Letter letter in letters)
            {
                if (letter.name == ".")
                    letter.name = ":";
                else if (letter.name == ",")
                    letter.name = ";";
                else if (letter.name == "-")
                    letter.name = "_";
                else if (letter.name == "'")
                    letter.name = "\"";
                else
                    letter.name = char.ToUpper(char.Parse(letter.name)).ToString();
                letter.visibleText.text = letter.name;
            }
        }
        else
        {
            foreach (Letter letter in letters)
            {
                if (letter.name == ":")
                    letter.name = ".";
                else if (letter.name == ";")
                    letter.name = ",";
                else if (letter.name == "_")
                    letter.name = "-";
                else if (letter.name == "\"")
                    letter.name = "'";
                else
                    letter.name = char.ToLower(char.Parse(letter.name)).ToString();
                letter.visibleText.text = letter.name;
            }
        }
        if (keyboard.window != null) keyboard.window.field.caretPosition = lcp;
    }
}
