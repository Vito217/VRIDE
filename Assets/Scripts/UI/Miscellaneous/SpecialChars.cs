using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpecialChars : VRKey
{
    public List<GameObject> letterRows;
    public List<GameObject> specialCharRows;

    public void OnClick()
    {
        if (name == "ABC")
        {
            foreach (GameObject row in specialCharRows) row.SetActive(false);
            foreach (GameObject row in letterRows) row.SetActive(true);
            name = "#&@";
            visibleText.text = "#&@";
        }
        else
        {
            foreach (GameObject row in specialCharRows) row.SetActive(true);
            foreach (GameObject row in letterRows) row.SetActive(false);
            name = "ABC";
            visibleText.text = "ABC";
        }
    }
}
