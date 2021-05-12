using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class WindowCube : MonoBehaviour
{
    public Transform face1;
    public Transform face2;
    public Transform face3;
    public Transform face4;
    public Transform face5;
    public Transform face6;

    public float movementSpeed = 5f;
    public float rotationSpeed = 1000f;
    public float scalingSpeed = 5f;

    private Transform baseParent;
    protected bool isDragged = false;
    private bool opened = false;
    private bool loading = false;

    void Start()
    {
        GetComponent<XRSimpleInteractable>().interactionManager =
            GameObject.Find("XR Interaction Manager").GetComponent<XRInteractionManager>();

        foreach(Canvas c in GetComponentsInChildren<Canvas>())
            c.worldCamera = Camera.main;
    }

    void Update()
    {
        
    }

    public void OnActivate()
    {
        if (!opened)
        {
            Open();
        }
        else
        {
            Close();
        }
    }

    private IEnumerator MoveLocallyToTarget(Transform face, Vector3 target)
    {
        while (face.localPosition != target)
        {
            face.localPosition = Vector3.MoveTowards(
                face.localPosition, target, Time.deltaTime * movementSpeed
            );

            yield return null;
        }
    }

    private IEnumerator RotateLocallyToTarget(Transform face, Vector3 eulerTarget)
    {
        while (face.transform.localRotation.eulerAngles != eulerTarget)
        {
            face.localRotation = Quaternion.Euler(
                Vector3.MoveTowards(
                    face.localRotation.eulerAngles, eulerTarget, Time.deltaTime * rotationSpeed
                )
            );

            yield return null;
        }
    }

    public void MoveLocallyTo(Transform face, Vector3 target) { StartCoroutine(MoveLocallyToTarget(face, target)); }
    public void RotateLocallyTo(Transform face, Vector3 target) { StartCoroutine(RotateLocallyToTarget(face, target)); }

    void Open()
    {
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

    public void OnSelectEnter(SelectEnterEventArgs eventArgs)
    {
        isDragged = true;
        //baseParent = transform.parent;
        transform.SetParent(eventArgs.interactor.transform);
    }

    public void OnSelectExit()
    {
        isDragged = false;
        transform.SetParent(null, true);
    }
}
