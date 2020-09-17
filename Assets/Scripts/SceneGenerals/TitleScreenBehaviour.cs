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
        if (XRSettings.loadedDeviceName.Contains("OpenVR") ||
            XRSettings.loadedDeviceName.Contains("Oculus"))
        {
            XRSettings.enabled = true;
            List<InputDevice> inputDevices = new List<InputDevice>();
            InputDevices.GetDevices(inputDevices);
            foreach (InputDevice device in inputDevices) if (device.isValid)
            {
                if(Regex.Match(device.name, @"VIVE|Vive|vive|HTC|htc").Success ||
                    Regex.Match(device.manufacturer, @"VIVE|Vive|vive|HTC|htc").Success)
                {
                    player = Instantiate(htcplayer_prefab);
                    ground.AddComponent<Teleportable>();
                    ground.GetComponent<Teleportable>().target = player.transform;
                    ground.GetComponent<Teleportable>().pivot = player.transform.Find("ViveCameraRig/Camera");
                }
                else
                {
                    player = Instantiate(openVRPlayerPrefab);
                    ground.AddComponent<TeleportArea>();
                }
                break;
            }
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