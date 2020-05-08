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

    public GameObject UIHelpers_prefab;
    public GameObject defaultEventSystem_prefab;

    //public GameObject default_camera;

    // Start is called before the first frame update
    void Start()
    {
        StartCoroutine(Coroutine());
    }

    IEnumerator Coroutine()
    {
        if(XRSettings.loadedDeviceName == "")
        {
            foreach (string s in XRSettings.supportedDevices)
            {
                XRSettings.LoadDeviceByName(s);
                if (XRSettings.loadedDeviceName != "")
                    break;
            }
        }
        
        GameObject new_player;
        GameObject eventSystem;

        switch (XRSettings.loadedDeviceName)
        {
            case "OpenVR":

                //Enable SteamVR (HTC Vive)
                yield return null;
                XRSettings.enabled = true;

                //Instantiate player
                new_player = Instantiate(htcplayer_prefab);
                new_player.transform.position = new Vector3(0.0f, 0.0f, 0.0f);

                //Instantiate Event System
                eventSystem = Instantiate(UIHelpers_prefab);

                break;

            case "Oculus":
                
                // Enable Oculus
                yield return null;
                XRSettings.enabled = true;
                
                // Instantiate player
                new_player = Instantiate(oculusplayer_prefab);
                new_player.transform.position = new Vector3(0.0f, 0.0f, 0.0f);

                // Instantiate Event System
                eventSystem = Instantiate(UIHelpers_prefab);

                break;

            default:

                eventSystem = Instantiate(defaultEventSystem_prefab);
                new_player = Instantiate(nonvrplayer_prefab);
                new_player.transform.position = new Vector3(0.0f, 0.0f, 0.0f);
                break;
        }

        yield return new WaitForSeconds(3);
        text.GetComponent<Text>().CrossFadeAlpha(0.0f, 3.0f, false);
        this_canvas.GetComponent<Image>().CrossFadeAlpha(0.0f, 3.0f, false);
        yield return new WaitForSeconds(3);
        gameObject.SetActive(false);
    }
}
