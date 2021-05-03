using UnityEngine;
using UnityEngine.UI;

public class CapsLockKey : VRKey
{
    public Toggle capsLockToggle;
    public static bool capsLockEnabled;

    void Start()
    {
        keycode = KeyCode.CapsLock;
    }

    void Update()
    {
        if (Input.GetKeyDown(keycode))
        {
            var colors = GetComponent<Button>().colors;
            colors.normalColor = colors.pressedColor;
            GetComponent<Button>().colors = colors;
            capsLockToggle.isOn = !capsLockToggle.isOn;
            capsLockEnabled = capsLockToggle.isOn;
        }
        else if (Input.GetKeyUp(keycode))
        {
            var colors = GetComponent<Button>().colors;
            colors.normalColor = colors.selectedColor;
            GetComponent<Button>().colors = colors;
        }
    }

    public override void OnClick()
    {
        capsLockToggle.isOn = !capsLockToggle.isOn;
        capsLockEnabled = capsLockToggle.isOn;
    }
}
