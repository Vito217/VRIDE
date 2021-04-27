using System.Collections;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.Networking;
using UnityEngine.UI;

public class DesktopView : InitializeBehaviour
{
    public Image img;
    public static string streamerIP;

    private bool click = false;
    private bool keepRequesting = true;
    private Vector2 coords = Vector2.zero;

    public override IEnumerator innerStart()
    {
        StartCoroutine(RequestForImage());
        return base.innerStart();
    }

    public void OnPointerClick(BaseEventData data)
    {
        click = true;
        Vector3 hitWorldPosition = ((PointerEventData)data).pointerCurrentRaycast.worldPosition;
        Vector2 hitLocalPosition = img.transform.InverseTransformPoint(hitWorldPosition);
        Vector2 delta = GetComponent<RectTransform>().sizeDelta / 2;
        coords = Vector2Int.RoundToInt(new Vector2(hitLocalPosition.x + delta.x, delta.y - hitLocalPosition.y));
    }

    IEnumerator RequestForImage()
    {
        bool firstRequest = true;
        while (keepRequesting)
        {
            string data = click + "." + coords.x + "." + coords.y;
            if (click) click = false;

            using (UnityWebRequest uwr = UnityWebRequest.Post(streamerIP, data))
            {
                yield return uwr.SendWebRequest();

                try
                {
                    byte[] result = uwr.downloadHandler.data;

                    if (firstRequest)
                    {
                        firstRequest = false;
                        Texture2D t = new Texture2D(1, 1); t.LoadImage(result);
                        img.sprite = Sprite.Create(t, new Rect(0, 0, t.width, t.height), new Vector2(t.width / 2, t.height / 2));
                        GetComponent<RectTransform>().sizeDelta = new Vector2(t.width, t.height);
                    }
                    else
                    {
                        img.sprite.texture.LoadImage(result);
                    }
                }
                catch { }
            }
        }
    }

    private void OnDestroy()
    {
        keepRequesting = false;
    }
}
