using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;
using LoggingModule;
using SaveAndLoad;
using PharoModule;
using InstantiatorModule;
using System.Text;
using System.Xml.Linq;
using System.Runtime.Serialization;
using System.Security.Cryptography;
using System.Diagnostics;
using System.Diagnostics.Eventing.Reader;
using System.Runtime.Serialization.Formatters.Binary;

public class VRIDEController : MonoBehaviour
{
    public Camera camera;
    public bool can_move = true;
    public static SystemData data;
    public static string transcriptContents = "";
    public List<Browser> browsers;
    public List<Playground> playgrounds;
    public List<Inspector> inspectors;
    public List<Graph> graphs;
    public List<Transcript> transcripts;

    void Start()
    {
        Pharo.Start();
        SaveAndLoadModule.Load(this);
        InteractionLogger.SessionStart();
    }

    // F1 : Browser
    // F2 : Playground
    // F3 : Do it
    // F4 : Print it
    // F5 : Inspect it
    // F6 : Accept
    // F7 : Transcript

    void Update()
    {
        bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
        bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
        bool f1 = Input.GetKeyDown(KeyCode.F1);
        bool f2 = Input.GetKeyDown(KeyCode.F2);
        bool f7 = Input.GetKeyDown(KeyCode.F7);
        bool o = Input.GetKeyDown("o");
        bool b = Input.GetKeyDown("b");
        bool w = Input.GetKeyDown("w");
        bool t = Input.GetKeyDown("t");

        if (f1 || f2 || f7 || leftCmd || leftCtrl)
        {
            Vector3 pos = transform.position;
            Vector3 forw = transform.forward;

            Vector3 newPos = new Vector3(pos.x + forw.x * 5f, 0f, pos.z + forw.z * 5f);
            Vector3 newFinalPos = new Vector3(newPos.x, 2f, newPos.z);
            Vector3 newForw = new Vector3(forw.x, 0, forw.z);

            if (f1 || ((leftCtrl || leftCmd) && o && b))
            {
                Browser browser = Instantiator.Browser(data) as Browser;
                browser.Initialize(newPos, new Vector3(newFinalPos.x, 2.25f, newFinalPos.z), newForw, this);
                browsers.Add(browser);
                InteractionLogger.Count("Browser");
            }
            else if (f2 || ((leftCtrl || leftCmd) && o && w))
            {
                Playground playground = Instantiator.Playground() as Playground;
                playground.Initialize(newPos, newFinalPos, newForw, this);
                playgrounds.Add(playground);
                InteractionLogger.Count("Playground");
            }
            else if (f7 || ((leftCtrl || leftCmd) && o && t))
            {
                Transcript transcript = Instantiator.Transcript() as Transcript;
                transcript.Initialize(newPos, newFinalPos, newForw, this);
                transcripts.Add(transcript);
                InteractionLogger.Count("Transcript");
            }
        }

        if (Input.GetKeyDown(KeyCode.Escape))
        {
            SaveAndLoadModule.Save(this);
            Pharo.Execute("SmalltalkImage current snapshot: true andQuit: true.");
            InteractionLogger.SessionEnd();
            Application.Quit();
        }  
    }
}
