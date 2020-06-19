using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class InspectorData : SerializerData
{
    public List<Tuple<string, string>> rows;

    public InspectorData(
        Vector3 pos, 
        Vector3 fwd,
        List<Tuple<string, string>> rws) : base(pos, fwd)
    {
        rows = rws;
    }
}
