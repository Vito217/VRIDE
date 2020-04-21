using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class ClassWindow : MonoBehaviour
{
    private BrowserClass last_selected_class = null;

    public BrowserClass getLastSelectedClass()
    {
        return last_selected_class;
    }

    public void setLastSelectedClass(BrowserClass c)
    {
        last_selected_class = c;
    }
}
