using System.Collections.Generic;
using UnityEngine;

public class PharoClassCodeCube : PharoCodeCube
{
    List<CodeCube> childCubes;

    private bool opened = false;

    public override void InnerStart()
    {
        childCubes = new List<CodeCube>();
        foreach (Transform child in transform)
            childCubes.Add(child.gameObject.GetComponent<CodeCube>());

        base.InnerStart();
    }

    public void OnActivate()
    {
        if (!opened)
        {
            opened = true;

            RotateLocally(360);
            ScaleTo(new Vector3(.4f, .4f, .4f));

            childCubes[0].MoveLocallyTo(new Vector3(1f, 0f, 0f));
            childCubes[0].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);

            childCubes[1].MoveLocallyTo(new Vector3(0f, 1f, 0f));
            childCubes[1].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);

            childCubes[2].MoveLocallyTo(new Vector3(0f, 0f, 1f));
            childCubes[2].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);

            childCubes[3].MoveLocallyTo(new Vector3(-1f, 0f, 0f));
            childCubes[3].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);

            childCubes[4].MoveLocallyTo(new Vector3(0f, -1f, 0f));
            childCubes[4].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);

            childCubes[5].MoveLocallyTo(new Vector3(0f, 0f, -1f));
            childCubes[5].transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
        }
        else
        {
            opened = false;

            RotateLocally(-360);
            ScaleTo(new Vector3(.5f, .5f, .5f));
            childCubes[0].MoveLocallyTo(new Vector3(0f, 0f, 0f));
            childCubes[1].MoveLocallyTo(new Vector3(0f, 0f, 0f));
            childCubes[2].MoveLocallyTo(new Vector3(0f, 0f, 0f));
            childCubes[3].MoveLocallyTo(new Vector3(0f, 0f, 0f));
            childCubes[4].MoveLocallyTo(new Vector3(0f, 0f, 0f));
            childCubes[5].MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }
    }
}
