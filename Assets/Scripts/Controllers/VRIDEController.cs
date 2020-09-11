using System.Collections;
using LoggingModule;
using SaveAndLoad;
using UnityEngine;
using HTC.UnityPlugin.Vive;

public class VRIDEController : MonoBehaviour
{
    public bool can_move = true;

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
        bool menuButton = 
            ViveInput.GetPressDownEx(HandRole.RightHand, ControllerButton.Menu) ||
            ViveInput.GetPressDownEx(HandRole.LeftHand, ControllerButton.Menu);

        // KeyBoard
        bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
        bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
        bool esc = Input.GetKeyDown(KeyCode.Escape);
        bool f1 = Input.GetKeyDown(KeyCode.F1);
        bool f2 = Input.GetKeyDown(KeyCode.F2);
        bool f7 = Input.GetKeyDown(KeyCode.F7);
        bool f9 = Input.GetKeyDown(KeyCode.F9);
        bool o = Input.GetKey("o");
        bool b = Input.GetKeyDown("b");
        bool w = Input.GetKeyDown("w");
        bool t = Input.GetKeyDown("t");

        pos = transform.position;
        forw = Camera.main.transform.forward;
        newPos = new Vector3(pos.x + forw.x * .5f, 0f, pos.z + forw.z * .5f);
        newFinalPos = new Vector3(newPos.x, .5f, newPos.z);
        newForw = new Vector3(forw.x, 0, forw.z);

        if (f1 || f2 || f7 || leftCmd || leftCtrl 
            || esc || f9 || menuButton)
        {
            if (f1 || ((leftCtrl || leftCmd) && o && b))
                GenerateBrowser();
            else if (f2 || ((leftCtrl || leftCmd) && o && w))
                GeneratePlayground();
            else if (f7 || ((leftCtrl || leftCmd) && o && t))
                GenerateTranscript();
            else if (f9 || menuButton)
                GenerateMenu();
        }
    }

    public void GenerateBrowser()
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize(newPos, newFinalPos, newForw);
        SaveAndLoadModule.browsers.Add(browser);
        InteractionLogger.Count("Browser");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GeneratePlayground()
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize(newPos, newFinalPos, newForw);
        SaveAndLoadModule.playgrounds.Add(playground);
        InteractionLogger.Count("Playground");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GenerateTranscript()
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize(newPos, newFinalPos, newForw);
        SaveAndLoadModule.transcripts.Add(transcript);
        InteractionLogger.Count("Transcript");
        if (menu != null) Destroy(menu.gameObject);
    }

    public void GenerateMenu()
    {
        if (menu != null) Destroy(menu.gameObject);
        menu = Instantiator.Instance.Menu();
        menu.Reset();
        menu.playgroundGenerator.onClick.AddListener(GeneratePlayground);
        menu.browserGenerator.onClick.AddListener(GenerateBrowser);
        menu.transcriptGenerator.onClick.AddListener(GenerateTranscript);
        menu.quit.onClick.AddListener(Exit);
        menu.Initialize(newPos, newFinalPos, newForw);
    }

    public void Exit()
    {
        SaveAndLoadModule.Save();
        InteractionLogger.SessionEnd();
        Application.Quit();
    }
}