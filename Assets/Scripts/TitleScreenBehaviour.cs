using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.XR;

public class TitleScreenBehaviour : MonoBehaviour
{
    public GameObject text;
    public GameObject this_canvas;

    public GameObject nonvrplayer_prefab;
    public GameObject oculusplayer_prefab;
    public GameObject htcplayer_prefab;
    public GameObject default_camera;

    // Start is called before the first frame update
    void Start()
    {
        StartCoroutine(Coroutine());
    }

    IEnumerator Coroutine()
    {

        foreach (string s in XRSettings.supportedDevices)
        {
            XRSettings.LoadDeviceByName(s);
            if (XRSettings.loadedDeviceName != "")
                break;
        }
        switch (XRSettings.loadedDeviceName)
        {
            case "OpenVR":
                yield return null;
                XRSettings.enabled = true;
                break;
            case "Oculus":
                yield return null;
                XRSettings.enabled = true;
                break;
            default:
                GameObject new_player = Instantiate(nonvrplayer_prefab);
                new_player.transform.position = new Vector3(0.0f, 0.5f, 0.0f);
                default_camera.SetActive(false);
                break;
        }

        yield return new WaitForSeconds(3);
        text.GetComponent<Text>().CrossFadeAlpha(0.0f, 3.0f, false);
        this_canvas.GetComponent<Image>().CrossFadeAlpha(0.0f, 3.0f, false);
        yield return new WaitForSeconds(3);
        gameObject.SetActive(false);
    }
}
