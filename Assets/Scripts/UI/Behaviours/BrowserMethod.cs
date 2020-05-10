using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class BrowserMethod : BrowserObject
{
    public void onSelectMethod()
    {
        field.text = sourceCode;
    }
}
