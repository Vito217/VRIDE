using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;
using LoggingModule;
using SaveAndLoad;
using PharoModule;

public class VRIDEController : MonoBehaviour
{
    public bool can_move = true;
    public static SystemData sysData;
    public static string transcriptContents = "";
    public List<Browser> browsers;
    public List<Playground> playgrounds;
    public List<Inspector> inspectors;
    public List<Graph> graphs;
    public List<Transcript> transcripts;

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
        bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
        bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
        bool f1 = Input.GetKeyDown(KeyCode.F1);
        bool f2 = Input.GetKeyDown(KeyCode.F2);
        bool f7 = Input.GetKeyDown(KeyCode.F7);
        bool o = Input.GetKey("o");
        bool b = Input.GetKeyDown("b");
        bool w = Input.GetKeyDown("w");
        bool t = Input.GetKeyDown("t");

        if (f1 || f2 || f7 || leftCmd || leftCtrl)
        {
            Vector3 pos = transform.position;
            Vector3 forw = transform.forward;
            Vector3 newPos = new Vector3(pos.x + forw.x * 5f, 0f, pos.z + forw.z * 5f);
            Vector3 newFinalPos = new Vector3(newPos.x, 2.25f, newPos.z);
            Vector3 newForw = new Vector3(forw.x, 0, forw.z);

            if (f1 || ((leftCtrl || leftCmd) && o && b))
                GenerateBrowser(newPos, newFinalPos, newForw);
            else if (f2 || ((leftCtrl || leftCmd) && o && w))
                GeneratePlayground(newPos, newFinalPos, newForw);
            else if (f7 || ((leftCtrl || leftCmd) && o && t))
                GenerateTranscript(newPos, newFinalPos, newForw);
        }

        if (Input.GetKeyDown(KeyCode.Escape))
        {
            SaveAndLoadModule.Save(this);
            //Pharo.Execute("SmalltalkImage current snapshot: true andQuit: true.");
            Pharo.Execute("SmalltalkImage current snapshot: true andQuit: false.");
            InteractionLogger.SessionEnd();
            Application.Quit();
        }
    }

    void GenerateBrowser(Vector3 newPos, Vector3 newFinalPos, Vector3 newForw)
    {
        Browser browser = Instantiator.Instance.Browser();
        browser.Initialize(newPos, newFinalPos, newForw, this);
        browsers.Add(browser);
        InteractionLogger.Count("Browser");
    }

    void GeneratePlayground(Vector3 newPos, Vector3 newFinalPos, Vector3 newForw)
    {
        Playground playground = Instantiator.Instance.Playground();
        playground.Initialize(newPos, newFinalPos, newForw, this);
        playgrounds.Add(playground);
        InteractionLogger.Count("Playground");
    }

    void GenerateTranscript(Vector3 newPos, Vector3 newFinalPos, Vector3 newForw)
    {
        Transcript transcript = Instantiator.Instance.Transcript();
        transcript.Initialize(newPos, newFinalPos, newForw, this);
        transcripts.Add(transcript);
        InteractionLogger.Count("Transcript");
    }
}