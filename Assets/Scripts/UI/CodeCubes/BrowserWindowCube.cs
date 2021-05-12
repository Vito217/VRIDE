using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class BrowserWindowCube : Browser
{
    void Start()
    {
        StartCoroutine(innerStart());   
    }

    void Update()
    {
        innerBehaviour();
    }
}
