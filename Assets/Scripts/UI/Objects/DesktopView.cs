using System.Collections;
using System.Net.Http;
using System.Text;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class DesktopView : InitializeBehaviour
{
    public Image img;
    public static string streamerIP;
    public static readonly HttpClient streamerClient = new HttpClient();

    private bool click = false;
    private bool keepRequesting = true;
    private Vector2 coords = Vector2.zero;

    public override IEnumerator innerStart()
    {
        ReadFromURL();
        return base.innerStart();
    }

    public void OnPointerClick(BaseEventData data)
    {
        click = true;
        Vector3 hitWorldPosition = ((PointerEventData)data).pointerCurrentRaycast.worldPosition;
        Vector2 hitLocalPosition = img.transform.InverseTransformPoint(hitWorldPosition);
        coords = Vector2Int.RoundToInt(hitLocalPosition + GetComponent<RectTransform>().sizeDelta);
    }

    async void ReadFromURL()
    {
        while (keepRequesting)
        {
            try
            {
                // Building the data
                HttpContent content = new ByteArrayContent(Encoding.UTF8.GetBytes(click + " " + coords.x + " " + coords.y));
                HttpResponseMessage response = await streamerClient.PostAsync(streamerIP, content);

                click = false;

                byte[] result = await response.Content.ReadAsByteArrayAsync();
                Texture2D tex = new Texture2D(1, 1);
                tex.LoadImage(result);

                float width = tex.width;
                float height = tex.height;

                //Updating window size
                GetComponent<RectTransform>().sizeDelta = new Vector2(width, height);

                // Updating image
                img.sprite = Sprite.Create(tex, new Rect(0, 0, width, height), new Vector2(width * .5f, height * .5f));
            }
            catch { }
        }
    }

    private void OnDestroy()
    {
        keepRequesting = false;
        streamerClient.Dispose();
    }
}
