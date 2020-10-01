using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.XR;
using UnityEngine.EventSystems;
using Valve.VR.InteractionSystem;
using PharoModule;
using SaveAndLoad;
using LoggingModule;
using System.Threading.Tasks;
using HTC.UnityPlugin.Vive;

public class TitleScreenBehaviour : MonoBehaviour
{
    public GameObject text;
    public Slider slider;
    bool initializing = true;
    float limit = 0.0f;

    private Dictionary<string, VRIDEController> dict;

    public VRIDEController htcplayer_prefab;
    public VRIDEController openVRPlayerPrefab;
    public VRIDEController nonvrplayer_prefab;
    public GameObject ground;

    void Update()
    {
        if (initializing)
            Load();

        slider.value += (limit - slider.value) * 0.01f;
    }

    async void Load()
    {
        VRIDEController player;
        List<InputDevice> inputDevices = new List<InputDevice>();
        InputDevices.GetDevices(inputDevices);
        if (inputDevices.Count > 0)
        {
            XRSettings.enabled = true;
            player = Instantiate(htcplayer_prefab);
            ground.AddComponent<Teleportable>();
            ground.GetComponent<Teleportable>().target = player.transform;
            ground.GetComponent<Teleportable>().pivot = player.transform.Find("ViveCameraRig/Camera");
            player.transform.Find("ViveCameraRig/RightHand").gameObject.AddComponent<CapsuleCollider>();
            player.transform.Find("ViveCameraRig/LefttHand").gameObject.AddComponent<CapsuleCollider>();
        }
        else
            player = Instantiate(nonvrplayer_prefab);

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