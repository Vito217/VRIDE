using LoggingModule;
using UnityEngine;
using System.Text.RegularExpressions;
using UnityEngine.XR.Interaction.Toolkit;
using System.Collections;
using System.IO;

public class VRIDEController : MonoBehaviour
{
    private VRIDEMenu menu;

    public Transform cameraTransform;
    public GameObject leftStick, rightStick;
    public GameObject leftHand, rightHand;
    public Transform leftTransform, rightTransform;
    public GameObject currentActivePointer;

    Vector3 currentPosition = Vector3.zero;

    void Start()
    {
        GetComponent<XRRig>().cameraYOffset = 1.5f;

        //StartCoroutine(PositionLogCorountine());
    }

    void Update()
    {
        if (Input.anyKeyDown && Regex.Match(Input.inputString, @"[a-zA-Z0-9]").Success && !InteractionLogger.isUsingPhysicalKeyboard) 
            InteractionLogger.RegisterPhysicalKeyboard();

        VRIDEInputHandler inputHandler = GetComponent<VRIDEInputHandler>();

        bool menuButton = (inputHandler.RightSecondaryButtonDown ||
                           inputHandler.LeftSecondaryButtonDown) && 
                          !(inputHandler.RightGrip || inputHandler.LeftGrip);

        bool rightTrigger = inputHandler.RightTrigger;
        bool leftTrigger = inputHandler.LeftTrigger;

        bool rightForwardAxis = inputHandler.RightAxisUp;
        bool leftForwardAxis = inputHandler.LeftAxisUp;
        bool rightBackAxis = inputHandler.RightAxisDown;
        bool leftBackAxis = inputHandler.LeftAxisDown;

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
        if (rightTrigger)
        {
            currentActivePointer = rightHand.transform.parent.gameObject;

            if (rightForwardAxis)
                MoveGrabbedObject(true, true);
            else if (rightBackAxis)
                MoveGrabbedObject(false, true);
        }

        if (leftTrigger)
        {
            currentActivePointer = leftHand.transform.parent.gameObject;

            if (leftForwardAxis)
                MoveGrabbedObject(true, false);
            else if (leftBackAxis)
                MoveGrabbedObject(false, false);
        }

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

    /// <summary>
    /// 
    /// </summary>
    public void ExchangeHandsAndSpheres()
    {
        // Spheres
        foreach (Renderer lRenderer in leftStick.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Renderer lRenderer in rightStick.GetComponentsInChildren<Renderer>()) 
            lRenderer.enabled = !lRenderer.enabled;
        foreach (Collider lCollider in leftStick.GetComponentsInChildren<Collider>()) 
            lCollider.enabled = !lCollider.enabled;
        foreach (Collider lCollider in rightStick.GetComponentsInChildren<Collider>()) 
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

    /// <summary>
    /// 
    /// </summary>
    /// <param name="isForward"></param>
    /// <param name="isRightHand"></param>
    void MoveGrabbedObject(bool isForward, bool isRightHand)
    {
        Transform hand = isRightHand ? rightTransform : leftTransform;
        InitializeBehaviour window = hand.gameObject.GetComponentInChildren<InitializeBehaviour>();
        if (window != null)
        {
            //Vector3 dir = (hand.position - window.transform.position).normalized;
            Vector3 dir = hand.forward;
            if (!isForward) dir *= -1f;
            window.transform.localPosition = window.transform.localPosition + dir  * .01f;
        }
    }

    /**
    IEnumerator PositionLogCorountine()
    {
        int time = 0;
        string logFile = Path.Combine(Application.persistentDataPath, "positionData.csv");

        if (File.Exists(logFile))
            File.Delete(logFile);
        File.AppendAllText(logFile, "X,Y,Z,Time\n");

        while (true)
        {
            File.AppendAllText(logFile, 
                cameraTransform.position.x.ToString().Replace(",", ".") + "," + 
                cameraTransform.position.y.ToString().Replace(",", ".") + "," + 
                cameraTransform.position.z.ToString().Replace(",", ".") + "," + time + "\n");

            time++;
            yield return new WaitForSeconds(1);
        }
    }
    **/
}