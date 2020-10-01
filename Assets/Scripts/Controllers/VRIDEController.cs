using System.Collections;
using LoggingModule;
using SaveAndLoad;
using UnityEngine;
using HTC.UnityPlugin.Vive;

public class VRIDEController : MonoBehaviour
{
    public bool can_move = true;
    public Transform dragPivot;

    Vector3 pos;
    Vector3 forw;
    Vector3 newPos;
    Vector3 newFinalPos;
    Vector3 newForw;
    VRIDEMenu menu;

    void Awake()
    {
        StartCoroutine(Coroutine());
    }

    // F1 : Browser
    // F2 : Playground
    // F3 : Do it
    // F4 : Print it
    // F5 : Inspect it
    // F6 : Accept
    // F7 : Transcript

    IEnumerator Coroutine()
    {
        Cursor.visible = false;
        Cursor.lockState = CursorLockMode.Locked;
        yield return 0;
    }

    void Update()
    {
        transform.position = new Vector3(
            transform.position.x, .5f, transform.position.z);

        // HTC VIVE

        // Right Hand
        bool rightMenuButton = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Menu);
        bool rightAKey = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.AKey) ||
                         ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.AKeyTouch);
        bool rightBKey = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.BKey);
        bool rightTrigger = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Trigger) ||
                            ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.TriggerTouch);
        bool rightPad = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Pad) ||
                        ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.PadTouch);
        bool rightBumper = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Bumper) ||
                           ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.BumperTouch);

        bool usingRightHand = rightAKey || rightBKey || rightTrigger || rightPad || rightBumper;

        // Left Hand
        bool leftMenuButton = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Menu);
        bool leftAKey = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.AKey) ||
                         ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.AKeyTouch);
        bool leftBKey = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.BKey);
        bool leftTrigger = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Trigger) ||
                            ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.TriggerTouch);
        bool leftPad = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Pad) ||
                        ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.PadTouch);
        bool leftBumper = ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Bumper) ||
                           ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.BumperTouch);

        bool usingLeftHand = leftAKey || leftBKey || leftTrigger || leftPad || leftBumper;

        // KeyBoard
        bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
        bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
        bool esc = Input.GetKeyDown(KeyCode.Escape);
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
            pos.x + forw.x * .5f, 
            pos.y, 
            pos.z + forw.z * .5f);
        newForw = new Vector3(forw.x, 0, forw.z);

        if (f1 || f2 || f7 || leftCmd || leftCtrl
            || esc || f8 || f9 || rightMenuButton || leftMenuButton)
        {
            if (f1 || ((leftCtrl || leftCmd) && o && b))
                GenerateBrowser();
            else if (f2 || ((leftCtrl || leftCmd) && o && w))
                GeneratePlayground();
            else if (f7 || ((leftCtrl || leftCmd) && o && t))
                GenerateTranscript();
            else if (f8)
                GenerateRoassalExamples();
            else if (f9 || rightMenuButton || leftMenuButton)
                GenerateMenu();
        }

        if (usingLeftHand)
            dragPivot = transform.Find("ViveCameraRig/LeftHand");
        else if (usingRightHand)
            dragPivot = transform.Find("ViveCameraRig/RightHand");
    }

    public void GenerateBrowser()
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GeneratePlayground()
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GenerateTranscript()
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize(newFinalPos, newForw);
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GenerateRoassalExamples()
    {
        RoassalExamples re = Instantiator.Instance.RoassalExamples();
        re.Initialize(newFinalPos, newForw);
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GenerateMenu()
    {
        if (menu != null) 
            Destroy(menu.gameObject);
        else
        {
            menu = Instantiator.Instance.Menu();
            menu.playgroundGenerator.onClick.AddListener(GeneratePlayground);
            menu.browserGenerator.onClick.AddListener(GenerateBrowser);
            menu.transcriptGenerator.onClick.AddListener(GenerateTranscript);
            menu.roassalGenerator.onClick.AddListener(GenerateRoassalExamples);
            menu.quit.onClick.AddListener(Exit);
            menu.Initialize(newFinalPos, newForw);
        }
    }

    public void Exit()
    {
        SaveAndLoadModule.Save();
        InteractionLogger.SessionEnd();
        Application.Quit();
    }
}