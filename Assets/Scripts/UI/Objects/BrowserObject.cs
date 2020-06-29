using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class BrowserObject : MonoBehaviour
{
    public string name;
    public string sourceCode;
    public TMP_InputField field;

    public void click()
    {
        GetComponent<Button>().onClick.Invoke();
    }
}
