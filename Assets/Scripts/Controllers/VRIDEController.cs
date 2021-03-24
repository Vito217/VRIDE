using LoggingModule;
using UnityEngine;
using System.Text.RegularExpressions;
using UnityEngine.XR.Interaction.Toolkit;

public class VRIDEController : MonoBehaviour
{
    public VRIDEMenu menu;
    public GameObject leftSphere, rightSphere;
    public GameObject leftHand, rightHand;

    Vector3 currentPosition = Vector3.zero;

    void Awake()
    {
        GetComponent<XRRig>().cameraYOffset = 1.5f;
    }

    void Update()
    {
        if (Input.anyKeyDown && Regex.Match(Input.inputString, @"[a-zA-Z0-9]").Success && !InteractionLogger.isUsingPhysicalKeyboard) 
            InteractionLogger.RegisterPhysicalKeyboard();

        bool menuButton = GetComponent<VRIDEInputHandler>().RightSecondaryButtonDown ||
                          GetComponent<VRIDEInputHandler>().LeftSecondaryButtonDown;

        bool rightTrigger = GetComponent<VRIDEInputHandler>().RightTrigger;
        bool leftTrigger = GetComponent<VRIDEInputHandler>().LeftTrigger;

        bool rightForwardAxis = GetComponent<VRIDEInputHandler>().RightAxisUp;
        bool leftForwardAxis = GetComponent<VRIDEInputHandler>().LeftAxisUp;
        bool rightBackAxis = GetComponent<VRIDEInputHandler>().RightAxisDown;
        bool leftBackAxis = GetComponent<VRIDEInputHandler>().LeftAxisDown;

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

        // Basic Commands
        if (f1 || (cmd && o && b))
            menu.GenerateBrowser();
        else if (f2 || (cmd && o && w))
            menu.GeneratePlayground();
        else if (f7 || (cmd && o && t))
            menu.GenerateTranscript();
        else if (f8)
            menu.GenerateRoassalExamples();
        else if (f9 || menuButton)
            GenerateMenu();

        // Grab interactions
        if (rightForwardAxis && rightTrigger)
            MoveGrabbedObject(true, true);
        else if (rightBackAxis && rightTrigger)
            MoveGrabbedObject(false, true);

        if (leftForwardAxis && leftTrigger)
            MoveGrabbedObject(true, false);
        else if (leftBackAxis && leftTrigger)
            MoveGrabbedObject(false, false);

        // Position log
        if (currentPosition != transform.position)
        {
            currentPosition = transform.position;
            InteractionLogger.RegisterPlayerPosition(currentPosition.x, currentPosition.y, currentPosition.z);
        }
    }

    /// <summary>
    /// Creates a menu, or destroys if it already exists
    /// </summary>
    public void GenerateMenu()
    {
        if (menu == null)
        {
            menu = Instantiator.Instance.Menu();
            menu.Initialize();
        }
        else
            Destroy(menu.gameObject);

        GetComponent<VRIDEInputHandler>().RightSecondaryButtonDown = false;
        GetComponent<VRIDEInputHandler>().LeftSecondaryButtonDown = false;
    }

    public void ExchangeHandsAndSpheres()
    {
        // Spheres
        foreach (Renderer lRenderer in leftSphere.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Renderer lRenderer in rightSphere.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Collider lCollider in leftSphere.GetComponentsInChildren<Collider>()) 
            lCollider.enabled = !lCollider.enabled;
        foreach (Collider lCollider in rightSphere.GetComponentsInChildren<Collider>()) 
            lCollider.enabled = !lCollider.enabled;

        // Hands
        foreach (Renderer lRenderer in leftHand.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Renderer lRenderer in rightHand.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Collider lCollider in leftHand.GetComponentsInChildren<Collider>()) 
            lCollider.enabled = !lCollider.enabled;
        foreach (Collider lCollider in rightHand.GetComponentsInChildren<Collider>()) 
            lCollider.enabled = !lCollider.enabled;
    }

    void MoveGrabbedObject(bool isForward, bool isRightHand)
    {
        GameObject hand = isRightHand ? rightHand : leftHand;
        InitializeBehaviour window = hand.GetComponentInChildren<InitializeBehaviour>();
        if (window != null)
        {
            Vector3 dir = -hand.transform.forward.normalized;
            if (isForward) dir = dir * -1f;
            window.transform.position = window.transform.position + dir;
        }
    }
}