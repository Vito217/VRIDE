using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PharoVarCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string varName;

    public override void InnerStart()
    {
        base.InnerStart();
    }
}
