using System.Collections;
using UnityEngine;
using HTC.UnityPlugin.Vive;

public class VRIDETitleController : MonoBehaviour
{
    public Transform dragPivot;

    void Start()
    {
        StartCoroutine(Coroutine());
    }

    IEnumerator Coroutine()
    {
        Cursor.visible = false;
        Cursor.lockState = CursorLockMode.Locked;
        yield return 0;
    }

    void Update()
    {
        InnerBehaviour();
    }

    public virtual void InnerBehaviour()
    {
        bool leftTrigger = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.TriggerTouch);
        bool rightTrigger = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.TriggerTouch);

        if (leftTrigger)
            dragPivot = transform.Find("ViveCameraRig/LeftHand");
        else if (rightTrigger)
            dragPivot = transform.Find("ViveCameraRig/RightHand");
    }
}
