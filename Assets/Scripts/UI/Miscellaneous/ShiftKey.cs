using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class ShiftKey : VRKey
{
    public static bool shiftEnabled;

    public void OnPointerDown(BaseEventData data)
    {
        shiftEnabled = true;
    }

    public void OnPointerUp(BaseEventData data)
    {
        shiftEnabled = false;
    }
}
