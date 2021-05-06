using System.Collections.Generic;
using UnityEngine;

public class PharoClassCodeCube : PharoCodeCube
{
    public List<Transform> childLists;
    private bool opened = false;

    public override void InnerStart()
    {

        base.InnerStart();
    }

    public void OnActivate()
    {
        if (!opened)
        {
            opened = true;

            RotateLocally(360);
            //ScaleTo(new Vector3(.4f, .4f, .4f));

            for(int i = 0; i < childLists[0].childCount; i++)
            {
                CodeCube child = childLists[0].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(i + 1, 0f, 0f));
            }

            for (int i = 0; i < childLists[1].childCount; i++)
            {
                CodeCube child = childLists[1].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(0f, i + 1, 0f));
            }

            for (int i = 0; i < childLists[2].childCount; i++)
            {
                CodeCube child = childLists[2].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(0f, 0f, i + 1));
            }

            for (int i = 0; i < childLists[3].childCount; i++)
            {
                CodeCube child = childLists[3].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(-(i + 1), 0f, 0f));
            }

            for (int i = 0; i < childLists[4].childCount; i++)
            {
                CodeCube child = childLists[4].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(0f, -(i + 1), 0f));
            }

            for (int i = 0; i < childLists[5].childCount; i++)
            {
                CodeCube child = childLists[5].GetChild(i).GetComponent<CodeCube>();
                child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
                child.MoveLocallyTo(new Vector3(0f, 0f, -(i + 1)));
            }
        }
        else
        {
            opened = false;

            RotateLocally(-360);
            //ScaleTo(new Vector3(.5f, .5f, .5f));

            for (int i = 0; i < childLists[0].childCount; i++)
            {
                childLists[0].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }

            for (int i = 0; i < childLists[1].childCount; i++)
            {
                childLists[1].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }

            for (int i = 0; i < childLists[2].childCount; i++)
            {
                childLists[2].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }

            for (int i = 0; i < childLists[3].childCount; i++)
            {
                childLists[3].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }

            for (int i = 0; i < childLists[4].childCount; i++)
            {
                childLists[4].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }

            for (int i = 0; i < childLists[5].childCount; i++)
            {
                childLists[5].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
            }
        }
    }
}
