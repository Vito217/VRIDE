using System.Collections;
using System.Net.Http;
using System.Text;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class DesktopView : InitializeBehaviour
{
    public static string streamerIP;
    public static readonly HttpClient streamerClient = new HttpClient();

    public Image img;
    private bool mouseOver = false;
    private bool mouseClicked = false;
    private GameObject pointer;
    private Vector2 coordinates = Vector2.zero; 

    bool keepRequesting = true;

    public override IEnumerator innerStart()
    {
        ReadFromURL();
        return base.innerStart();
    }

    public override void innerBehaviour()
    {
        base.innerBehaviour();
        if (mouseOver) OnHover();
    }

    void OnHover()
    {
        LineRenderer line = pointer.GetComponent<LineRenderer>();
        Vector3 endPoint = line.GetPosition(1);
        Vector3 hitLocalPosition = img.transform.InverseTransformPoint(endPoint);

        var sizeDelta = GetComponent<RectTransform>().sizeDelta;
        Vector2 mouseCoords = new Vector2(hitLocalPosition.x, hitLocalPosition.y) + sizeDelta;
        coordinates = mouseCoords;

        VRIDEInputHandler inputs = pointer.transform.root.GetComponent<VRIDEInputHandler>();
        mouseClicked = inputs.LeftTrigger || inputs.RightTrigger;
    }

    public void OnPointerEnter(BaseEventData data)
    {
        mouseOver = true;
        Transform player = ((PointerEventData)data).enterEventCamera.transform.root;
        pointer = player.GetComponent<VRIDEController>().currentActivePointer;
    }

    public void OnPointerExit(BaseEventData data)
    {
        mouseOver = false;
    }

    async void ReadFromURL()
    {
        while (keepRequesting)
        {
            try
            {
                // Building the data
                string isMouseOver = mouseOver.ToString();
                string isMouseClicked = mouseClicked.ToString();
                string xCoord = Mathf.Round(coordinates.x).ToString();
                string yCoord = Mathf.Round(coordinates.y).ToString();
                string sContent = isMouseOver + " " + isMouseClicked + " " + xCoord + " " + yCoord;

                HttpContent content = new ByteArrayContent(Encoding.UTF8.GetBytes(sContent));

                HttpResponseMessage response = await streamerClient.PostAsync(streamerIP, content);
                byte[] result = await response.Content.ReadAsByteArrayAsync();

                Texture2D tex = new Texture2D(1, 1);
                tex.LoadImage(result);

                float width = tex.width;
                float height = tex.height;

                //Updating window size
                GetComponent<RectTransform>().sizeDelta = new Vector2(width, height);

                // Updating image
                Sprite sprite = Sprite.Create(tex, new Rect(0, 0, width, height), new Vector2(width * .5f, height * .5f));
                img.sprite = sprite;
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
