using LoggingModule;
using SaveAndLoad;
using UnityEngine;
using System.Text.RegularExpressions;
using UnityEngine.UI;

public class VRIDEController : MonoBehaviour
{
    public VRIDEMenu menu;

    Vector3 pos;
    Vector3 forw;
    Vector3 newFinalPos;
    Vector3 newForw;

    Vector3 currentPosition = Vector3.zero;

    //public List<Material> skyBoxes;
    //int skyboxesIndex = 0;

    void Update()
    {
        if (Input.anyKeyDown && Regex.Match(Input.inputString, @"[a-zA-Z0-9]").Success && !InteractionLogger.isUsingPhysicalKeyboard) 
            InteractionLogger.RegisterPhysicalKeyboard();

        // HTC VIVE
        bool menuButton = GetComponent<VRIDEInputHandler>().RightSecondaryButtonDown ||
                          GetComponent<VRIDEInputHandler>().LeftSecondaryButtonDown;

        //bool skyButton = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.AKey) ||
        //                 ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.AKey);

        // KeyBoard
        bool cmd = Input.GetKey(KeyCode.LeftCommand) ||
                   Input.GetKey(KeyCode.RightCommand) ||
                   Input.GetKey(KeyCode.LeftControl) ||
                   Input.GetKey(KeyCode.RightControl);

        bool f1 = Input.GetKeyDown(KeyCode.F1);
        bool f2 = Input.GetKeyDown(KeyCode.F2);
        bool f7 = Input.GetKeyDown(KeyCode.F7);
        bool f8 = Input.GetKeyDown(KeyCode.F8);
        bool f9 = Input.GetKeyDown(KeyCode.F9);
        bool o = Input.GetKey("o");
        bool b = Input.GetKeyDown("b");
        bool w = Input.GetKeyDown("w");
        bool t = Input.GetKeyDown("t");

        pos = Camera.main.transform.position;
        forw = Camera.main.transform.forward;
        newFinalPos = new Vector3(
            pos.x + forw.x * .8f,
            .9f * pos.y,
            pos.z + forw.z * .8f);
        newForw = new Vector3(forw.x, 0, forw.z);

        if (f1 || f2 || f7 || cmd || f8 || f9 || menuButton)
        {
            if (f1 || (cmd && o && b))
                GenerateBrowser();
            else if (f2 || (cmd && o && w))
                GeneratePlayground();
            else if (f7 || (cmd && o && t))
                GenerateTranscript();
            else if (f8)
                GenerateRoassalExamples();
            else if (f9 || menuButton)
                GenerateMenu();
            //else if (skyButton)
            //{
            //    skyboxesIndex = (skyboxesIndex + 1) % skyBoxes.Count;
            //    RenderSettings.skybox = skyBoxes[skyboxesIndex];
            //}
        }

        if (currentPosition != transform.position)
        {
            currentPosition = transform.position;
            InteractionLogger.RegisterPlayerPosition(currentPosition.x, currentPosition.y, currentPosition.z);
        }        
    }

    public void GenerateBrowser()
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser", browser.GetInstanceID().ToString());

        if (menu.transform.Find("Button Collection/Settings/Viewport/Content/Keyboard Enable").gameObject.GetComponent<Toggle>().isOn)
            browser.ToggleKeyboard();

        menu.gameObject.SetActive(false);
    }

    public void GeneratePlayground()
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground", playground.GetInstanceID().ToString());

        if (menu.transform.Find("Button Collection/Settings/Viewport/Content/Keyboard Enable").gameObject.GetComponent<Toggle>().isOn)
            playground.ToggleKeyboard();

        menu.gameObject.SetActive(false);
    }

    public void GenerateTranscript()
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript", transcript.GetInstanceID().ToString());
        menu.gameObject.SetActive(false);
    }

    public void GenerateRoassalExamples()
    {
        RoassalExamples re = Instantiator.Instance.RoassalExamples();
        re.Initialize(newFinalPos, newForw);
        menu.gameObject.SetActive(false);
    }

    public void GenerateWebcam()
    {
        WebcamView wc = Instantiator.Instance.WebCam();
        wc.Initialize(newFinalPos, newForw);
        menu.gameObject.SetActive(false);
    }

    public void GenerateMenu()
    {
        menu.gameObject.SetActive(!menu.gameObject.activeSelf);
        if(menu.gameObject.activeSelf) menu.Initialize(newFinalPos, newForw);
    }

    public void Exit()
    {
        SaveAndLoadModule.Save();
        InteractionLogger.SessionEnd();
        Application.Quit();
    }
}