/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

************************************************************************************/
using UnityEngine;
using UnityEngine.EventSystems;

public class ButtonDownListener : MonoBehaviour, IPointerDownHandler
{
    public event System.Action onButtonDown;

    public void OnPointerDown(PointerEventData eventData)
    {
        if (onButtonDown != null)
        {
            onButtonDown.Invoke();
        }
    }
}
