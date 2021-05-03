using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class VRKey : MonoBehaviour
{
    public KeyCode keycode;

    void Update()
    {
        if (keycode != KeyCode.None)
        {
            if (Input.GetKeyDown(keycode))
            {
                var colors = GetComponent<Button>().colors;
                colors.normalColor = colors.pressedColor;
                GetComponent<Button>().colors = colors;
            }
            else if (Input.GetKeyUp(keycode))
            {
                var colors = GetComponent<Button>().colors;
                colors.normalColor = colors.selectedColor;
                GetComponent<Button>().colors = colors;
            }
        }
    }

    public virtual void OnClick() { }
}
