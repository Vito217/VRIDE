using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CodeCube : MonoBehaviour
{
    public float movementSpeed = 2f;
    public float rotationSpeed = 2f;
    public float scalingSpeed = 2f;

    List<CodeCube> childCubes;

    // Start is called before the first frame update
    void Start()
    {
        childCubes = new List<CodeCube>();
        foreach (Transform child in transform)
            childCubes.Add(child.gameObject.GetComponent<CodeCube>());
    }

    // Update is called once per frame
    void Update()
    {
        
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
            transform.position = Vector3.MoveTowards(
                transform.position, target, Time.deltaTime * movementSpeed
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
            transform.Rotate(0f, 0f, degreesRotated);
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

    public void OnClick()
    {
        RotateLocally(360);
        ScaleTo(new Vector3(.4f, .4f, .4f));
        childCubes[0].MoveLocallyTo(new Vector3(1f, 0f, 0f));
        childCubes[1].MoveLocallyTo(new Vector3(0f, 1f, 0f));
        childCubes[2].MoveLocallyTo(new Vector3(0f, 0f, 1f));
        childCubes[3].MoveLocallyTo(new Vector3(-1f, 0f, 0f));
        childCubes[4].MoveLocallyTo(new Vector3(0f, -1f, 0f));
        childCubes[5].MoveLocallyTo(new Vector3(0f, 0f, -1f));
    }
}
