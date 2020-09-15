using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class RoassalObject : MonoBehaviour
{
    public RoassalExamples roassal;

    void Start()
    {
        GetComponent<TextMeshProUGUI>().text = name;
    }

    public void click()
    {
        onSelect();
    }

    public virtual void onSelect()
    {
        GetComponent<TextMeshProUGUI>().color = roassal.skyBlue;
    }

    public void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = roassal.white;
    }

    public void onEnterPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == roassal.skyBlue))
            GetComponent<TextMeshProUGUI>().color = roassal.gray;
    }

    public void onExitPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == roassal.skyBlue))
            GetComponent<TextMeshProUGUI>().color = roassal.white;
    }
}
