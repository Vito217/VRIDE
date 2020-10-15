using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine;
using UnityEngine.UI;

public class AFrameGeometry : MonoBehaviour
{
    Transform baseParent;

    public void OnDrag(BaseEventData data)
    {
        Camera theCamera = ((PointerEventData)data).enterEventCamera;
        VRIDEController thePlayer = theCamera.transform.root.gameObject.GetComponent<VRIDEController>();
        baseParent = transform.parent;
        transform.SetParent(thePlayer.dragPivot);
    }

    public void OnEndDrag(BaseEventData data)
    {
        Vector3 new_pos = transform.parent.TransformPoint(transform.localPosition);
        transform.SetParent(baseParent);
        transform.position = new_pos;
    }
}
