using UnityEngine;
using UnityEngine.UI;
using LoggingModule;
using SaveAndLoad;
using System.Collections;
using UnityEngine.EventSystems;
using TMPro;
using PharoModule;
using System.Net;

public class VRIDEMenu : InitializeBehaviour
{
    private static IPHostEntry host = Dns.GetHostEntry(Dns.GetHostName());
    private static string localhost = host.AddressList[host.AddressList.Length - 1].ToString();

    public static string pharoIPState = localhost;
    public static string streamerIPState = localhost;
    public static string pharoPortState = "1701";
    public static string streamerPortState = "5000";
    public static bool enableSpeechRecognition = false;
    public static bool vrHandsToggleState = false;

    public Toggle keyboardToggle;
    public Toggle vrHandsToggle;

    public GameObject lastSelected;
    public Material spaceSkyBox;
    public Material forestSkyBox;
    public Material defaultSkyBox;

    public TMP_InputField pharoIP;
    public TMP_InputField pharoPort;

    public TMP_InputField streamerIP;
    public TMP_InputField streamerPort;

    public override IEnumerator innerStart()
    {
        vrHandsToggle.isOn = vrHandsToggleState;

        pharoIP.text = pharoIPState;
        pharoPort.text = pharoPortState;

        streamerIP.text = streamerIPState;
        streamerPort.text = streamerPortState;

        UpdatePharoIPAndPort();
        UpdateStreamerIPAndPort();

        return base.innerStart();
    }

    public void GenerateBrowser()
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize();
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser", browser.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GeneratePlayground()
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize();
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground", playground.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateTranscript()
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize();
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript", transcript.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateRoassalExamples()
    {
        RoassalExamples re = Instantiator.Instance.RoassalExamples();
        re.Initialize();

        InteractionLogger.Count("RoassalExamples", re.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateWebcam()
    {
        WebcamView wc = Instantiator.Instance.WebCam();
        wc.Initialize();

        InteractionLogger.Count("Webcam", wc.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateBoard()
    {
        Board board = Instantiator.Instance.Board();
        board.Initialize();

        InteractionLogger.Count("Board", board.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateFileExplorer()
    {
        FileExplorer explorer = Instantiator.Instance.FileExplorer();
        explorer.Initialize();

        InteractionLogger.Count("FileExplorer", explorer.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateDesktopWindowsExplorer()
    {
        DesktopWindowsExplorer dv = Instantiator.Instance.DesktopWindowsExplorer();
        dv.Initialize();

        InteractionLogger.Count("DesktopWindowsExplorer", dv.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateVirtualKeyBoard()
    {
        Keyboards vk = Instantiator.Instance.VirtualKeyboard();
        vk.Initialize();

        InteractionLogger.Count("VirtualKeyboard", vk.GetInstanceID().ToString());

        //Destroy(gameObject);
    }
    public void GenerateVirtualKeyBoard2()
    {
        Keyboards vk = Instantiator.Instance.VirtualKeyboard2();
        vk.Initialize();

        InteractionLogger.Count("VirtualKeyboard2", vk.GetInstanceID().ToString());

        //Destroy(gameObject);
    }

    public void GenerateBrowserWindowCube()
    {
        BrowserWindowCube bwc = Instantiator.Instance.BrowserWindowCube();
        bwc.Initialize();

        //Destroy(gameObject);
    }

    public void GeneratePlaygroundWindowCube()
    {
        PlaygroundWindowCube bwc = Instantiator.Instance.PlaygroundWindowCube();
        bwc.Initialize();

        //Destroy(gameObject);
    }

    public void Exit()
    {
        SaveAndLoadModule.Save();
        InteractionLogger.SessionEnd();
        Application.Quit();
    }

    public void ChangeEnvToSpace()
    {
        RenderSettings.skybox = spaceSkyBox;

        Instantiator.currentEnvironment.SetActive(false);
        Instantiator.currentEnvironment = Instantiator.Instance.spaceShip;
        Instantiator.currentEnvironment.SetActive(true);

        foreach (VRIDEController user in FindObjectsOfType<VRIDEController>())
            user.transform.position = Vector3.zero;

        Initialize();
    }

    public void ChangeEnvToForest()
    {
        RenderSettings.skybox = forestSkyBox;

        Instantiator.currentEnvironment.SetActive(false);
        Instantiator.currentEnvironment = Instantiator.Instance.forest;
        Instantiator.currentEnvironment.SetActive(true);

        foreach (VRIDEController user in FindObjectsOfType<VRIDEController>())
            user.transform.position = Vector3.zero;

        Initialize();
    }

    public void ChangeEnvToDefault()
    {
        RenderSettings.skybox = defaultSkyBox;

        Instantiator.currentEnvironment.SetActive(false);
        Instantiator.currentEnvironment = Instantiator.Instance.defaultGround;
        Instantiator.currentEnvironment.SetActive(true);

        foreach (VRIDEController user in FindObjectsOfType<VRIDEController>())
            user.transform.position = Vector3.zero;

        Initialize();
    }

    public void VRHandsToggle(BaseEventData data)
    {
        vrHandsToggleState = vrHandsToggle.isOn;
        Transform player = ((PointerEventData)data).enterEventCamera.transform.root;
        player.gameObject.GetComponent<VRIDEController>().ExchangeHandsAndSpheres();
    }

    public void UpdatePharoIPAndPort()
    {
        pharoIPState = pharoIP.text;
        pharoPortState = pharoPort.text;
        Pharo.pharoIP = "http://" + pharoIP.text + ":" + pharoPort.text + "/repl";
    }

    public void UpdateStreamerIPAndPort()
    {
        streamerIPState = streamerIP.text;
        streamerPortState = streamerPort.text;
        DesktopWindowsExplorer.streamerIP = "http://" + streamerIP.text + ":" + streamerPort.text + "/";
    }
}
