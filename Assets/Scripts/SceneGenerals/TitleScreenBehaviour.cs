using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.XR;
using UnityEngine.XR.Management;
using PharoModule;
using SaveAndLoad;
using LoggingModule;
using System.Threading.Tasks;
using HTC.UnityPlugin.Vive;

public class TitleScreenBehaviour : MonoBehaviour
{
    public GameObject text;
    public Slider slider;
    float limit = 0.0f;
    bool initializing = true;

    private Dictionary<string, VRIDEController> dict;

    public VRIDEController htcplayer_prefab;
    public VRIDEController openVRPlayerPrefab;
    public VRIDEController nonvrplayer_prefab;
    public GameObject ground;

    void Update()
    {
        if (initializing)
            Init();
        slider.value += (limit - slider.value) * 0.01f;
    }

    async void Init()
    {
        initializing = false;

        // VR PLAYER
        VRIDEController player = Instantiate(htcplayer_prefab);
        ground.AddComponent<Teleportable>();
        ground.GetComponent<Teleportable>().target = player.transform;
        ground.GetComponent<Teleportable>().pivot = player.transform.Find("ViveCameraRig/Camera");
        
        // NON VR PLAYER
        //Instantiate(nonvrplayer_prefab);

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

        await Task.Delay(3000);
        
        gameObject.SetActive(false);
    }
}