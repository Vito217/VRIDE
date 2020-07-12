using UnityEngine;

[System.Serializable]
public class SerializerData
{
    public (float x, float y, float z) position;
    public (float x, float y, float z) forward;

    public SerializerData(Vector3 pos, Vector3 fwd)
    {
        position = (pos.x, pos.y, pos.z);
        forward = (fwd.x, fwd.y, fwd.z);
    }
}
