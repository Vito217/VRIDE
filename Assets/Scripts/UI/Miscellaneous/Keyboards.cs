using System.Collections;
using UnityEngine;
using UnityEngine.EventSystems;

public class Keyboards : MonoBehaviour
{
    Transform baseParent;

    void Start()
    {
        baseParent = transform.parent;
    }

    void Update()
    {
        transform.localRotation = Quaternion.Euler(0, 0, 0);
    }

    public void GrabKeyboard(BaseEventData data)
    {
        Transform player = ((PointerEventData)data).enterEventCamera.transform.root;
        StartCoroutine(HandleDrag(player));
    }

    IEnumerator HandleDrag(Transform player)
    {
        yield return null;

        if (player.gameObject.GetComponent<VRIDEInputHandler>().LeftTrigger)
            transform.SetParent(player.Find("Camera Offset/LeftHand Controller"));
        else if (player.gameObject.GetComponent<VRIDEInputHandler>().RightTrigger)
            transform.SetParent(player.Find("Camera Offset/RightHand Controller"));
    }

    public void ReleaseKeyboard()
    {
        Vector3 gp = transform.position;
        Quaternion r = transform.rotation;

        transform.SetParent(baseParent);
        transform.position = gp;
        transform.rotation = r;
    }
}
