using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PharoPackageCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string packageName;

    public List<PharoClassCodeCube> classes;
}
