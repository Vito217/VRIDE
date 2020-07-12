using UnityEngine;

[System.Serializable]
public class PlaygroundData : SerializerData
{
    public string sourceCode;

    public PlaygroundData(Vector3 pos, Vector3 fwd, string sc)
        : base(pos, fwd)
    {
        sourceCode = sc;
    }
}
