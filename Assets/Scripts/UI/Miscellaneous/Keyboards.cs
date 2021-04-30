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

    public void Initialize()
    {
        Vector3 pos = Camera.main.transform.position;
        Vector3 forw = Camera.main.transform.forward;
        Vector3 newFinalPos = new Vector3(pos.x + forw.x * .8f, .9f * pos.y, pos.z + forw.z * .8f);
        Vector3 newForw = new Vector3(forw.x, 0, forw.z);
        transform.position = newFinalPos;
        transform.forward = newForw;
    }

    public void OnClose()
    {
        Destroy(gameObject);
    }
}
