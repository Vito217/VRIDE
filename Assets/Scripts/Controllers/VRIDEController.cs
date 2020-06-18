using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;
using LoggingModule;
using System.Security.Cryptography;

public class VRIDEController : MonoBehaviour
{
    public InitializeBehaviour browser_prefab;
    public InitializeBehaviour playground_prefab;
    private InitializeBehaviour og_browser;
    public Camera camera;
    public bool can_move = true;

    void Start()
    {
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
                    new_window = Instantiate(browser_prefab);
                    og_browser = new_window;
                }
                else
                    new_window = Instantiate(og_browser);
                InteractionLogger.Count("Browser");
            }
            else
            {
                new_window = Instantiate(playground_prefab);
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
            InteractionLogger.SessionEnd();
            Application.Quit();
        }  
    }
}
