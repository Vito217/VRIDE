using UnityEngine;
using UnityEngine.UI;
using LoggingModule;
using SaveAndLoad;
using System.Collections;

public class VRIDEMenu : InitializeBehaviour
{
    public static bool keyboardToggleState;
    public static bool vrHandsToggleState;

    public Toggle keyboardToggle;
    public Toggle vrHandsToggle;

    public GameObject lastSelected;
    public Material spaceSkyBox;
    public Material forestSkyBox;
    public Material defaultSkyBox;

    public override IEnumerator innerStart()
    {
        keyboardToggle.isOn = keyboardToggleState;
        vrHandsToggle.isOn = vrHandsToggleState;
        return base.innerStart();
    }

    public void GenerateBrowser()
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize();
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser", browser.GetInstanceID().ToString());

        if (keyboardToggle.isOn)
            browser.ToggleKeyboard();

        Destroy(gameObject);
    }

    public void GeneratePlayground()
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize();
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground", playground.GetInstanceID().ToString());

        if (keyboardToggle.isOn)
            playground.ToggleKeyboard();

        Destroy(gameObject);
    }

    public void GenerateTranscript()
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize();
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript", transcript.GetInstanceID().ToString());

        Destroy(gameObject);
    }

    public void GenerateRoassalExamples()
    {
        RoassalExamples re = Instantiator.Instance.RoassalExamples();
        re.Initialize();

        Destroy(gameObject);
    }

    public void GenerateWebcam()
    {
        WebcamView wc = Instantiator.Instance.WebCam();
        wc.Initialize();

        Destroy(gameObject);
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

    public void KeyboardToggle()
    {
        keyboardToggleState = keyboardToggle.isOn;
    }

    public void VRHandsToggle()
    {
        vrHandsToggleState = vrHandsToggle.isOn;
        if (vrHandsToggleState) GameObject.Find("XR Rig").GetComponent<VRIDEController>().EnableHands();
        else GameObject.Find("XR Rig").GetComponent<VRIDEController>().EnableSpheres();
    }
}
