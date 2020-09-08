using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class VRKey : MonoBehaviour
{
    public VRKeyboard keyboard;
    public TextMeshProUGUI visibleText;

    public virtual void OnClick() { }

    void OnCollisionEnter(Collision collision)
    {
        if (collision.gameObject.tag == "Hand")
        {
            Color c;
            ColorUtility.TryParseHtmlString("#8C8C8C", out c);
            var colors = GetComponent<Button>().colors;
            colors.normalColor = c;
            GetComponent<Button>().colors = colors;
            OnClick();
        }  
    }

    void OnCollisionExit(Collision collision)
    {
        if (collision.gameObject.tag == "Hand")
        {
            Color c;
            ColorUtility.TryParseHtmlString("#000000", out c);
            var colors = GetComponent<Button>().colors;
            colors.normalColor = c;
            GetComponent<Button>().colors = colors;
        }
    }
}
