using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class BrowserData : SerializerData
{
    public string lastSelectedPackage;
    public string lastSelectedClass;
    public string lastSelectedSide;

    public BrowserData(Vector3 pos, Vector3 fwd, string lsc, string lsp, string lss) 
        : base(pos, fwd)
    {
        lastSelectedClass = lsc;
        lastSelectedPackage = lsp;
        lastSelectedSide = lss;
    }
}
