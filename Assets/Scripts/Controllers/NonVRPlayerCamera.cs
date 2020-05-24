using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Security.Cryptography;
using UnityEngine;

public class NonVRPlayerCamera : MonoBehaviour
{
    public float sensitivity = 100f;
    public Transform playerBody;
    public float xRotation = 0f;
    public float yRotation = 0f;
    public float delta = 10f;

    // Start is called before the first frame update
    void Start()
    {
        //Cursor.visible = false;
        //Cursor.lockState = CursorLockMode.Confined;
    }

    // Update is called once per frame
    void Update()
    {
        //float mouseX = Input.GetAxis("Mouse X") * sensitivity * Time.deltaTime;
        //float mouseY = Input.GetAxis("Mouse Y") * sensitivity * Time.deltaTime;
        //xRotation -= mouseY;

        Vector3 mouse_pos = Input.mousePosition;

        if (mouse_pos.x <= 0f + delta || mouse_pos.x <= 0f - delta)
            yRotation = -1f;
        else if (mouse_pos.x >= Screen.width + delta || mouse_pos.x >= Screen.width - delta)
            yRotation = 1f;
        else
            yRotation = 0f;

        if (mouse_pos.y <= 0f + delta || mouse_pos.y <= 0f - delta)
            xRotation += 1f;
        else if (mouse_pos.y >= Screen.height + delta || mouse_pos.y >= Screen.height - delta)
            xRotation -= 1f;

        //xRotation -= 1;
        xRotation = Mathf.Clamp(xRotation, -90f, 90f);
        transform.localRotation = Quaternion.Euler(xRotation, 0f, 0f);
        playerBody.Rotate(Vector3.up * yRotation);

        //Cursor.lockState = (Cursor.lockState == CursorLockMode.Locked) ? CursorLockMode.None : CursorLockMode.Locked;
    }
}