using UnityEngine;
using UnityEngine.EventSystems;
using TMPro;

public class AFrameGeometry : InitializeBehaviour
{
    public Color baseColor;
    public Color hoverColor;
    public TextMeshPro t;

    public void OnPointerEnter(BaseEventData data) 
    {
        GetComponent<Renderer>().material.color = hoverColor;
        if (t != null) t.gameObject.SetActive(true);
    }

    public void OnPointerExit(BaseEventData data)
    {
        GetComponent<Renderer>().material.color = baseColor;
        if (t != null) t.gameObject.SetActive(false);
    }
}
