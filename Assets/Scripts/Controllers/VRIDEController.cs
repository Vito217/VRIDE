using LoggingModule;
using SaveAndLoad;
using UnityEngine;
using HTC.UnityPlugin.Vive;
using System.Text.RegularExpressions;

public class VRIDEController : MonoBehaviour
{
    public bool can_move = true;
    public GameObject quad;

    Vector3 pos;
    Vector3 forw;
    Vector3 newFinalPos;
    Vector3 newForw;
    VRIDEMenu menu;
    GameObject taskList;

    void Update()
    {
        if (Input.anyKeyDown && Regex.Match(Input.inputString, @"[a-zA-Z0-9]").Success && !InteractionLogger.isUsingPhysicalKeyboard) 
            InteractionLogger.RegisterPhysicalKeyboard();

        // HTC VIVE
        bool menuButton = ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Menu) ||
                          ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Menu);

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
        }
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

    public void GenerateTaskList()
    {
        if(taskList == null) taskList = Instantiator.Instance.TaskList();
        taskList.GetComponent<InitializeBehaviour>().Initialize(newFinalPos, newForw);
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
            menu.taskGenerator.onClick.AddListener(GenerateTaskList);
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