using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class BrowserData : SerializerData
{
    public string lastSelectedPackage;
    public string lastSelectedClass;

    public BrowserData(Vector3 pos, Vector3 fwd, string lsc, string lsp) 
        : base(pos, fwd)
    {
        lastSelectedClass = lsc;
        lastSelectedPackage = lsp;
    }
}
