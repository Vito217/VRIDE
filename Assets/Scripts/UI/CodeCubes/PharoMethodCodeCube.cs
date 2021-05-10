using TMPro;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class PharoMethodCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string methodName;
    [HideInInspector]
    public string code;
    [HideInInspector]
    public bool isInstance;

    public CodeCubeText codeCubeTextPrefab;

    public override void InnerStart()
    {
        base.InnerStart();
    }

    public void OnHoverEnter(HoverEnterEventArgs args)
    {
        if (!isDragged)
        {
            CodeCubeText text = Instantiate(codeCubeTextPrefab, transform);
            text.GetComponent<TextMeshPro>().text = isInstance ? "instanceMethod: " + methodName : "classMethod: " + methodName;
        }
    }

    public void OnHoverExit(HoverExitEventArgs args)
    {
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }
}
