using LoggingModule;
using UnityEngine;
using System.Text.RegularExpressions;

public class VRIDEController : MonoBehaviour
{
    public VRIDEMenu menu;
    Vector3 currentPosition = Vector3.zero;

    void Update()
    {
        if (Input.anyKeyDown && Regex.Match(Input.inputString, @"[a-zA-Z0-9]").Success && !InteractionLogger.isUsingPhysicalKeyboard) 
            InteractionLogger.RegisterPhysicalKeyboard();

        // HTC VIVE
        bool menuButton = GetComponent<VRIDEInputHandler>().RightSecondaryButtonDown ||
                          GetComponent<VRIDEInputHandler>().LeftSecondaryButtonDown;

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

        if (f1 || f2 || f7 || cmd || f8 || f9 || menuButton)
        {
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
        }

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
            (Vector3 newFinalPos, Vector3 newForw) = menu.GetPosAndForw();
            menu.Initialize(newFinalPos, newForw);
        }
        else
            Destroy(menu.gameObject);
    }
}