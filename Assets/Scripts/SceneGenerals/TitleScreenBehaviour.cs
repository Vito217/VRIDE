using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.XR;
using UnityEngine.EventSystems;
using Valve.VR.InteractionSystem;
using PharoModule;
using SaveAndLoad;
using LoggingModule;
using System.Threading.Tasks;

public class TitleScreenBehaviour : MonoBehaviour
{
    public GameObject text;
    public Slider slider;
    bool initializing = true;
    float limit = 0.0f;

    private Dictionary<string, VRIDEController> dict;

    public VRIDEController htcplayer_prefab;
    public VRIDEController nonvrplayer_prefab;
    public CustomStandaloneInputModule csim;
    public StandaloneInputModule sim;
    public TeleportArea ground;
    // public GameObject oculusplayer_prefab;
    // public GameObject UIHelpers_prefab;

    void Update()
    {
        if (initializing)
            Load();

        slider.value += (limit - slider.value) * 0.01f;
    }

    async void Load()
    {
        dict = new Dictionary<string, VRIDEController>() {
                { "" , nonvrplayer_prefab },
                { "OpenVR", htcplayer_prefab }
            };

        VRIDEController player = Instantiate(dict[XRSettings.loadedDeviceName]);
        player.transform.position = new Vector3(0.0f, 0.0f, 0.0f);
        if (XRSettings.loadedDeviceName == "OpenVR")
        {
            XRSettings.enabled = true;
            ground.enabled = true;
            csim.enabled = false;
            sim.enabled = true;
        }

        initializing = false;

        limit = 0.3f;

        await Pharo.Start();

        limit = 0.6f;

        await SaveAndLoadModule.Load();

        limit = 1.0f;

        InteractionLogger.SessionStart();

        await Task.Delay(5000);

        slider.gameObject.SetActive(false);
        text.SetActive(true);
        text.GetComponent<Text>().CrossFadeAlpha(0.0f, 3.0f, false);
        GetComponent<Image>().CrossFadeAlpha(0.0f, 3.0f, false);
    }
}