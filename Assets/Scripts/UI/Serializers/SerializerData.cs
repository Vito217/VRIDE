using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class SerializerData
{
    public (float x, float y, float z) position;
    public (float x, float y, float z) forward;

    public SerializerData(Vector3 pos, Vector3 fwd)
    {
        position = (x: pos.x, y: pos.y, z: pos.z);
        forward = (x: fwd.x, y: fwd.y, z: fwd.z);
    }
}
