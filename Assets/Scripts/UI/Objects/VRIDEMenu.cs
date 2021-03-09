using UnityEngine;
using UnityEngine.UI;
using LoggingModule;
using SaveAndLoad;

public class VRIDEMenu : InitializeBehaviour
{
    public GameObject lastSelected;
    public Material spaceSkyBox;
    public Material defaultSkyBox;

    public void GenerateBrowser()
    {
        (Vector3 newFinalPos, Vector3 newForw) = GetPosAndForw();
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser", browser.GetInstanceID().ToString());

        if (transform.Find("Button Collection/Settings/Viewport/Content/Keyboard Enable").gameObject.GetComponent<Toggle>().isOn)
            browser.ToggleKeyboard();

        gameObject.SetActive(false);
    }

    public void GeneratePlayground()
    {
        (Vector3 newFinalPos, Vector3 newForw) = GetPosAndForw();
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground", playground.GetInstanceID().ToString());

        if (transform.Find("Button Collection/Settings/Viewport/Content/Keyboard Enable").gameObject.GetComponent<Toggle>().isOn)
            playground.ToggleKeyboard();

        gameObject.SetActive(false);
    }

    public void GenerateTranscript()
    {
        (Vector3 newFinalPos, Vector3 newForw) = GetPosAndForw();
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript", transcript.GetInstanceID().ToString());
        gameObject.SetActive(false);
    }

    public void GenerateRoassalExamples()
    {
        (Vector3 newFinalPos, Vector3 newForw) = GetPosAndForw();
        RoassalExamples re = Instantiator.Instance.RoassalExamples();
        re.Initialize(newFinalPos, newForw);
        gameObject.SetActive(false);
    }

    public void GenerateWebcam()
    {
        (Vector3 newFinalPos, Vector3 newForw) = GetPosAndForw();
        WebcamView wc = Instantiator.Instance.WebCam();
        wc.Initialize(newFinalPos, newForw);
        gameObject.SetActive(false);
    }

    public void Exit()
    {
        SaveAndLoadModule.Save();
        InteractionLogger.SessionEnd();
        Application.Quit();
    }

    public (Vector3 newFinalPos, Vector3 newForw) GetPosAndForw()
    {
        Vector3 pos = Camera.main.transform.position;
        Vector3 forw = Camera.main.transform.forward;
        Vector3 newFinalPos = new Vector3(
            pos.x + forw.x * .8f,
            .9f * pos.y,
            pos.z + forw.z * .8f);
        Vector3 newForw = new Vector3(forw.x, 0, forw.z);
        return (newFinalPos, newForw);
    }

    public void ChangeEnvToSpace()
    {
        RenderSettings.skybox = spaceSkyBox;

        Instantiator.currentEnvironment.SetActive(false);
        Instantiator.currentEnvironment = Instantiator.Instance.spaceShip;
        Instantiator.currentEnvironment.SetActive(true);

        foreach (VRIDEController user in FindObjectsOfType<VRIDEController>())
            user.transform.position = Vector3.zero;
    }

    public void ChangeEnvToDefault()
    {
        RenderSettings.skybox = defaultSkyBox;

        Instantiator.currentEnvironment.SetActive(false);
        Instantiator.currentEnvironment = Instantiator.Instance.defaultGround;
        Instantiator.currentEnvironment.SetActive(true);

        foreach (VRIDEController user in FindObjectsOfType<VRIDEController>())
            user.transform.position = Vector3.zero;
    }
}
