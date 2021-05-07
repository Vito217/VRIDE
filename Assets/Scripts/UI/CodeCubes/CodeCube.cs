using System.Collections;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class CodeCube : MonoBehaviour
{
    public float movementSpeed = 2f;
    public float rotationSpeed = 1000f;
    public float scalingSpeed = 2f;

    private Transform baseParent;
   
    // Start is called before the first frame update
    void Start()
    {
        InnerStart();
    }

    public virtual void InnerStart()
    {
        GetComponent<XRSimpleInteractable>().interactionManager = GameObject.Find("XR Interaction Manager").GetComponent<XRInteractionManager>();
    }

    public void RotateLocally(float degrees)
    {
        StartCoroutine(RotateLocallyBy(degrees));
    }

    public void MoveLocallyTo(Vector3 target)
    {
        StartCoroutine(MoveLocallyToTarget(target));
    }

    public void ScaleTo(Vector3 target)
    {
        StartCoroutine(ScaleBy(target));
    }

    private IEnumerator MoveLocallyToTarget(Vector3 target)
    {
        while(transform.localPosition != target)
        {
            transform.localPosition = Vector3.MoveTowards(
                transform.localPosition, target, Time.deltaTime * movementSpeed
            );

            yield return null;
        }
    }

    private IEnumerator RotateLocallyBy(float degrees)
    {
        float degreeCounter = 0f;
        while(Mathf.Abs(degreeCounter) <= Mathf.Abs(degrees))
        {
            float degreesRotated = rotationSpeed * Time.deltaTime;
            transform.Rotate(0f, degreesRotated, 0f);
            degreeCounter += degreesRotated;
            yield return null;
        }
    }

    private IEnumerator ScaleBy(Vector3 target)
    {
        while (transform.localScale != target)
        {
            transform.localScale = Vector3.MoveTowards(
                transform.localScale, target, Time.deltaTime * scalingSpeed
            );

            yield return null;
        }
    }

    public void OnSelectEnter(SelectEnterEventArgs eventArgs)
    {
        baseParent = transform.parent;
        transform.SetParent(eventArgs.interactor.transform);
    }

    public void OnSelectExit()
    {
        transform.SetParent(baseParent);
    }
}
