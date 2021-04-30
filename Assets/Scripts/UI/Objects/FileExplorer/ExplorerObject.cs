using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class ExplorerObject : MonoBehaviour
{
    [HideInInspector]
    public FileExplorer explorer;

    [HideInInspector]
    public string fullPath;

    public void click()
    {
        onSelect();
    }

    public virtual void onSelect()
    {
        GetComponent<TextMeshProUGUI>().color = explorer.skyBlue;
    }

    public void onDeselect()
    {
        GetComponent<TextMeshProUGUI>().color = explorer.white;
    }

    public void onEnterPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == explorer.skyBlue))
            GetComponent<TextMeshProUGUI>().color = explorer.gray;
    }

    public void onExitPointer()
    {
        if (!(GetComponent<TextMeshProUGUI>().color == explorer.skyBlue))
            GetComponent<TextMeshProUGUI>().color = explorer.white;
    }
}
