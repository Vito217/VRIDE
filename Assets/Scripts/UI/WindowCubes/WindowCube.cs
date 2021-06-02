using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class WindowCube : CodeCube
{
    public Transform face1;
    public Transform face2;
    public Transform face3;
    public Transform face4;
    public Transform face5;
    public Transform face6;

    private bool opened = false;

    public override void InnerStart()
    {
        foreach (Canvas c in GetComponentsInChildren<Canvas>())
            c.worldCamera = Camera.main;
    }

    public void OnActivate()
    {
        if (!opened) Open();
        else Close();
    }

    void Open()
    {
        transform.rotation = Quaternion.Euler(Vector3.zero);

        MoveLocallyTo(face2, new Vector3(0f, 1f, -.5f));
        MoveLocallyTo(face3, new Vector3(0f, -1f, -.5f));
        MoveLocallyTo(face4, new Vector3(-1f, 0f, -.5f));
        MoveLocallyTo(face5, new Vector3(1f, 0f, -.5f));
        MoveLocallyTo(face6, new Vector3(2f, 0f, -.5f));

        face2.localRotation = Quaternion.Euler(Vector3.zero);
        face3.localRotation = Quaternion.Euler(Vector3.zero);
        face4.localRotation = Quaternion.Euler(Vector3.zero);
        face5.localRotation = Quaternion.Euler(Vector3.zero);
        face6.localRotation = Quaternion.Euler(Vector3.zero);

        opened = true;
    }

    void Close()
    {
        transform.rotation = Quaternion.Euler(Vector3.zero);

        MoveLocallyTo(face2, new Vector3(0f, .5f, 0f));
        MoveLocallyTo(face3, new Vector3(0f, -.5f, 0f));
        MoveLocallyTo(face4, new Vector3(-.5f, 0f, 0f));
        MoveLocallyTo(face5, new Vector3(.5f, 0f, 0f));
        MoveLocallyTo(face6, new Vector3(0f, 0f, .5f));

        face2.localRotation = Quaternion.Euler(90f, 0f, 0f);
        face3.localRotation = Quaternion.Euler(-90f, 0f, 0f);
        face4.localRotation = Quaternion.Euler(0f, 90f, 0f);
        face5.localRotation = Quaternion.Euler(0f, -90f, 0f);
        face6.localRotation = Quaternion.Euler(0f, 180f, 0f);

        opened = false;
    }
}
