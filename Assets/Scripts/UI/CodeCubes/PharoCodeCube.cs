using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class PharoCodeCube : CodeCube
{
    public override void InnerStart()
    {
        base.InnerStart();
    }

    public override void OnSelectEnter(SelectEnterEventArgs eventArgs)
    {
        base.OnSelectEnter(eventArgs);
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }
}
