using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class IPAddressTextField : MonoBehaviour
{
    public Button submitButton;

    // Update is called once per frame
    void Update()
    {
        if (GetComponent<TMP_InputField>().text.Contains("\n"))
        {
            GetComponent<TMP_InputField>().text = GetComponent<TMP_InputField>().text.Replace("\n", "");
            submitButton.onClick.Invoke();
        }
    }
}
