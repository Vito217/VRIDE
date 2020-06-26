using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PackageWindow : MonoBehaviour
{
    public BrowserPackage last_selected_package = null;

    public BrowserPackage getLastSelectedPackage()
    {
        return last_selected_package;
    }

    public void setLastSelectedPackage(BrowserPackage p)
    {
        last_selected_package = p;
    }
}
