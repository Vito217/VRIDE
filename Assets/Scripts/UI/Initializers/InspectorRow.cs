using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class InspectorRow : MonoBehaviour
{
    public TextMeshProUGUI var_button;
    public TextMeshProUGUI val_button;

    public void setContent(string var, string val, Transform itsParent)
    {
        var_button.text = var;
        val_button.text = val;
        gameObject.name = var;
        transform.SetParent(itsParent, false);
    }
}
