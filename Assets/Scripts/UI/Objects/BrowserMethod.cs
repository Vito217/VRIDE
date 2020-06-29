using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using TMPro;

public class BrowserMethod : BrowserObject
{
    void Start()
    {
        if(sourceCode == "")
        {
            sourceCode =
                "<b>messageSelectorAndArgumentNames</b>\n" +
                    "    | temporary variable names |\n" +
                    "    statements";
        }
    }

    public void onSelectMethod()
    {
        field.text = sourceCode;
    }
}
