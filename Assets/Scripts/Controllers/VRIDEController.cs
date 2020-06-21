using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;
using LoggingModule;
using SaveAndLoad;
using InstantiatorModule;
using System.Text;
using System.Xml.Linq;
using System.Runtime.Serialization;
using System.Security.Cryptography;
using System.Diagnostics.Eventing.Reader;
using System.Runtime.Serialization.Formatters.Binary;

public class VRIDEController : MonoBehaviour
{
    public InitializeBehaviour og_browser = null;
    public Camera camera;
    public bool can_move = true;

    public List<GameObject> browsers = new List<GameObject>();
    public List<GameObject> playgrounds = new List<GameObject>();
    public List<GameObject> inspectors = new List<GameObject>();
    public List<GameObject> graphs = new List<GameObject>();

    void Start()
    {
        SaveAndLoadModule.Load(this);
        InteractionLogger.SessionStart();
    }

    // F1 : Browser
    // F2 : Playground
    // F3 : Do it
    // F4 : Print it
    // F5 : Inspect it

    void Update()
    {
        if (Input.GetKeyDown(KeyCode.F1) || Input.GetKeyDown(KeyCode.F2))
        {
            InitializeBehaviour new_window;
            Vector3 pos = transform.position;
            Vector3 forw = transform.forward;
            Vector3 newWinPos = new Vector3(
                pos.x + forw.x * 5f,
                0f,
                pos.z + forw.z * 5f
            );
            if (Input.GetKeyDown(KeyCode.F1)){
                if (og_browser == null)
                {
                    new_window = Instantiator.Browser();
                    og_browser = new_window;
                }
                else
                    new_window = Instantiate(og_browser);
                browsers.Add(new_window.gameObject);
                InteractionLogger.Count("Browser");
            }
            else
            {
                new_window = Instantiator.Playground();
                playgrounds.Add(new_window.gameObject);
                InteractionLogger.Count("Playground");
            }
            new_window.Initialize(
                newWinPos,
                new Vector3(newWinPos.x, 2f, newWinPos.z),
                new Vector3(forw.x, 0, forw.z),
                gameObject
            );
        }

        if (Input.GetKeyDown(KeyCode.Escape))
        {
            SaveAndLoadModule.Save(this);
            InteractionLogger.SessionEnd();
            Application.Quit();
        }  
    }
}
