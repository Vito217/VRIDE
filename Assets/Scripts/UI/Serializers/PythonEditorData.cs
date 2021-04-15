using UnityEngine;

[System.Serializable]
public class PythonEditorData : SerializerData
{
    public string filename;
    public string fullpath;
    public string code;
    public string output;

    public PythonEditorData(Vector3 pos, Vector3 fwd, string name, string path, string theCode, string theOutput)
        : base(pos, fwd)
    {
        filename = name;
        fullpath = path;
        code = theCode;
        output = theOutput;
    }
}
