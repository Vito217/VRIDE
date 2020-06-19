using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class SVGData : SerializerData
{
    public string rawImage;
    public string type;

    public SVGData(Vector3 pos, Vector3 fwd, string ri, string tp)
        : base(pos, fwd)
    {
        rawImage = ri;
        type = tp;
    }
}
