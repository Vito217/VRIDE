using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class BrowserClass : BrowserObject
{
    public GameObject parent_window;
    public GameObject method_list;

    public void onSelectClass()
    {
        field.text = sourceCode;
        BrowserClass last_class = parent_window.GetComponent<ClassWindow>().getLastSelectedClass();
        if(last_class != null)
            last_class.onDeselectClass();
        parent_window.GetComponent<ClassWindow>().setLastSelectedClass(this);
        method_list.SetActive(true);
    }

    public void onDeselectClass()
    {
        method_list.SetActive(false);
    }
}
