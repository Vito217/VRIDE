using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using TMPro;
using PharoModule;

public class InspectorRow : MonoBehaviour
{
    public Inspector theInspector;
    public TextMeshProUGUI var_button;
    public TextMeshProUGUI val_button;

    public void setContent(string var, string val, Transform itsParent, Inspector ins)
    {
        theInspector = ins;
        var_button.text = var;
        val_button.text = val;
        gameObject.name = var;
        transform.SetParent(itsParent, false);
    }

    public async void onClickingObject(TextMeshProUGUI val)
    {
        string ob = val.text;
        string res = await Pharo.Inspect(ob);
        if (!Regex.Match(res, @"\[Error\](.*)").Success)
            theInspector.setContent(res);
    }
}
