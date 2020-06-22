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
using System.Diagnostics.Eventing.Reader;
using System.Runtime.Serialization.Formatters.Binary;

public class VRIDEController : MonoBehaviour
{
    public Camera camera;
    public bool can_move = true;
    public static SystemData data = new SystemData();
    public List<GameObject> browsers = new List<GameObject>();
    public List<GameObject> playgrounds = new List<GameObject>();
    public List<GameObject> inspectors = new List<GameObject>();
    public List<GameObject> graphs = new List<GameObject>();

    void Start()
    {
        string enginePath = Application.streamingAssetsPath + "PharoEngine/";
        string pharoPath = enginePath + "pharo";
        string imagePath = enginePath + "vride.image";
        string serverPath = enginePath + "server.st";
        string command = String.Format("{0} {1} st {2}", pharoPath, imagePath, serverPath);
        string bashFile = Application.platform == RuntimePlatform.WindowsPlayer ? "C:/Windows/System32/bash.exe" : "/bin/bash";

        var process = new Process()
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = bashFile,
                Arguments = $"-c \"{command}\"",
                RedirectStandardOutput = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            }
        };
        process.Start();

        SaveAndLoadModule.Load(this);
        InteractionLogger.SessionStart();
    }

    // F1 : Browser
    // F2 : Playground
    // F3 : Do it
    // F4 : Print it
    // F5 : Inspect it
    // F6 : Accept

    void Update()
    {
        if (Input.GetKeyDown(KeyCode.F1) || Input.GetKeyDown(KeyCode.F2))
        {
            Vector3 pos = transform.position;
            Vector3 forw = transform.forward;

            Vector3 newWinPos = new Vector3(pos.x + forw.x * 5f, 0f, pos.z + forw.z * 5f);
            Vector3 newWinFinalPos = new Vector3(newWinPos.x, 2f, newWinPos.z);
            Vector3 newForward = new Vector3(forw.x, 0, forw.z);

            if (Input.GetKeyDown(KeyCode.F1)){
                BrowserInit browser = Instantiator.Browser(data) as BrowserInit;
                browser.Initialize(newWinPos, newWinFinalPos, newForward, gameObject);
                browsers.Add(browser.gameObject);
                InteractionLogger.Count("Browser");
            }
            else
            {
                PlaygroundInit playground = Instantiator.Playground() as PlaygroundInit;
                playground.Initialize(newWinPos, newWinFinalPos, newForward, gameObject);
                playgrounds.Add(playground.gameObject);
                InteractionLogger.Count("Playground");
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
