using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PharoMethodCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string methodName;
    [HideInInspector]
    public string code;

    public override void InnerStart()
    {
        base.InnerStart();
    }
}
