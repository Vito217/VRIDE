using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToolbarBehaviour : MonoBehaviour
{
    public GameObject window;

    public void onClose()
    {
        Destroy(window);
    }
}
