using TMPro;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class PharoVarCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string varName;
    [HideInInspector]
    public bool isInstance;

    [HideInInspector]
    public string classType;

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

            if (!string.IsNullOrWhiteSpace(classType))
                text.GetComponent<TextMeshPro>().text += "\ntype: " + classType;
        }
    }

    public void OnHoverExit(HoverExitEventArgs args)
    {
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }
}
