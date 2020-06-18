using System.Collections;
using System.Collections.Generic;
using System.Security.Cryptography;
using UnityEngine;
using LoggingModule;
using System.Collections.Specialized;

public class ToolbarBehaviour : MonoBehaviour
{
    public GameObject window;
    public GameObject player;
    private bool dragging = false;
    private Vector3 rel_pos;
    private Vector3 rel_fwd;

    public void onClose()
    {
        InteractionLogger.Discount(window.name.Replace("(Clone)", ""));
        Destroy(window);
    }

    public void onDrag()
    {
        rel_pos = player.transform.InverseTransformPoint(window.transform.position);
        rel_fwd = player.transform.InverseTransformDirection(window.transform.forward);
        dragging = true;
    }

    public void onEndDrag()
    {
        dragging = false;
    }

    void Update()
    {
        if (dragging)
            dragAction();
    }

    private void dragAction()
    {
        Vector3 new_pos = player.transform.TransformPoint(rel_pos);
        Vector3 new_forw = player.transform.TransformDirection(rel_fwd);

        window.transform.position = Vector3.MoveTowards(
            window.transform.position,
            new_pos,
            0.15f
        );

        window.transform.forward = new_forw;
    }
}
