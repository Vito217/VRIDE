using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class InfoWindow : MonoBehaviour
{
    public Transform playerCamera;

    // Start is called before the first frame update
    void Start()
    {
        if(playerCamera != null)
            transform.position = new Vector3(
                transform.position.x, playerCamera.position.y + 1f, transform.position.z);
    }
}
