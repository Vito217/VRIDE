using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;
using TMPro;

public class AFrameGeometry : MonoBehaviour
{
    public Color baseColor;
    public Color hoverColor;
    public TextMeshPro t;

    private void Awake()
    {
        GetComponent<XRGrabInteractable>().interactionManager =
            GameObject.Find("XR Interaction Manager").GetComponent<XRInteractionManager>();
    }

    public void OnSelectEnter()
    {
        GetComponent<Rigidbody>().constraints = RigidbodyConstraints.None;
    }

    public void OnSelectExit()
    {
        GetComponent<Rigidbody>().constraints = RigidbodyConstraints.FreezeAll;
    }

    public void OnPointerEnter() 
    {
        GetComponent<Renderer>().material.color = hoverColor;
        if (t != null) t.gameObject.SetActive(true);
    }

    public void OnPointerExit()
    {
        GetComponent<Renderer>().material.color = baseColor;
        if (t != null) t.gameObject.SetActive(false);
    }
}
