using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using HTC.UnityPlugin.Vive;

public class VRIDETitleController : MonoBehaviour
{
    public Transform dragPivot;

    void Awake()
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
        transform.position = new Vector3(transform.position.x, .5f, transform.position.z);

        bool leftTrigger = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Trigger);
        bool rightTrigger = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Trigger);

        if (leftTrigger)
            dragPivot = transform.Find("ViveCameraRig/LeftHand");
        else if (rightTrigger)
            dragPivot = transform.Find("ViveCameraRig/RightHand");
    }
}
