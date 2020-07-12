using UnityEngine;

[System.Serializable]
public class InspectorData : SerializerData
{
    public string rows;

    public InspectorData(
        Vector3 pos, 
        Vector3 fwd,
        string rws) : base(pos, fwd)
    {
        rows = rws;
    }
}
