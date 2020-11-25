using UnityEngine;
using UnityEngine.UI;
using TMPro;
using System.Collections;

public class IPAddressTextField : MonoBehaviour
{
    public TMP_InputField nextField;
    public Button submitButton;

    // Update is called once per frame
    void Update()
    {
        if (GetComponent<TMP_InputField>().text.Contains("\t"))
        {
            GetComponent<TMP_InputField>().text =
                GetComponent<TMP_InputField>().text.Replace("\t", "");
            nextField.ActivateInputField();
            nextField.caretPosition = nextField.text.Length;
            nextField.selectionAnchorPosition = nextField.text.Length;
        }
        else if (GetComponent<TMP_InputField>().text.Contains("\n"))
        {
            GetComponent<TMP_InputField>().text =
                GetComponent<TMP_InputField>().text.Replace("\n", "");
            submitButton.onClick.Invoke();
        }
    }
}
