using TMPro;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class PharoVarCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string varName;
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
            text.GetComponent<TextMeshPro>().text = isInstance ? " instanceVar: " + varName : "classVar: " + varName;
        }
    }

    public void OnHoverExit(HoverExitEventArgs args)
    {
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }
}
