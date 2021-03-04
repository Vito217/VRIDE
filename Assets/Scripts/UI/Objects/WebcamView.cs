using UnityEngine;

public class WebcamView : InitializeBehaviour
{
#if UNITY_EDITOR_WIN || UNITY_EDITOR_LINUX || UNITY_STANDALONE_WIN || UNITY_STANDALONE_LINUX
    WebCamTexture webcamTexture;
#endif
    public GameObject quad;

    // Start is called before the first frame update
    void Start()
    {
#if UNITY_EDITOR_WIN || UNITY_EDITOR_LINUX || UNITY_STANDALONE_WIN || UNITY_STANDALONE_LINUX
        GetComponent<Canvas>().worldCamera = Camera.main;
        Application.RequestUserAuthorization(UserAuthorization.WebCam);
        webcamTexture = new WebCamTexture();
        quad.GetComponent<Renderer>().material.mainTexture = webcamTexture;
        webcamTexture.Play();
#endif
    }

    public override void innerBehaviour()
    {
        Vector2 sd = GetComponent<RectTransform>().sizeDelta;
        quad.transform.localScale = new Vector3(sd.x, sd.y, 1);
        quad.GetComponent<RectTransform>().sizeDelta = sd;
    }
}
