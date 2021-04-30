using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MenuButton : MonoBehaviour
{
    public VRIDEMenu menu;
    public GameObject associatedWindow;


    // Update is called once per frame
    public void OnClick()
    {
        if (menu.lastSelected != null)
            menu.lastSelected.SetActive(false);
        associatedWindow.SetActive(true);
        menu.lastSelected = associatedWindow;
    }
}
