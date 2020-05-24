using System.Collections;
using System.Collections.Generic;
using System.Security.Cryptography;
using UnityEngine;

public class ToolbarBehaviour : MonoBehaviour
{
    public GameObject window;
    public GameObject player;
    private bool dragging = false;

    public void onClose()
    {
        Destroy(window);
    }

    public void onDrag()
    {
        //Cursor.lockState = CursorLockMode.Locked;
        //Cursor.visible = false;
        dragging = true;
    }

    public void onEndDrag()
    {
        //Cursor.lockState = CursorLockMode.None;
        //Cursor.visible = true;
        dragging = false;
    }

    void Update()
    {
        if (dragging)
            dragAction();
    }

    private void dragAction()
    {
        Vector3 new_pos = player.transform.position + player.transform.forward * 5.0f;
        new_pos = new Vector3(new_pos.x, window.transform.position.y, new_pos.z);
        Vector3 new_forw = new Vector3(player.transform.forward.x, 0.0f, player.transform.forward.z);

        window.transform.position = Vector3.MoveTowards(
            window.transform.position,
            new_pos,
            0.5f
            );

        window.transform.forward = new_forw;
    }
}
