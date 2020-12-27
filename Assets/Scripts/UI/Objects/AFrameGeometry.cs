using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;

public class AFrameGeometry : InitializeBehaviour
{
    public Color baseColor;
    public Color hoverColor;

    public void OnPointerEnter(BaseEventData data) 
    {
        GetComponent<Renderer>().material.color = hoverColor;
    }

    public void OnPointerExit(BaseEventData data)
    {
        GetComponent<Renderer>().material.color = baseColor;
    }
}
