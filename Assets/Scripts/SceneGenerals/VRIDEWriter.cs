using System.IO;
using System.Collections.Generic;

public class VRIDEWriter : StreamWriter
{
    public readonly Queue<string> buffer;

    public VRIDEWriter(Stream s) : base(s)
	{
        buffer = new Queue<string>();
	}

    public override void Write(string value)
    {
        buffer.Enqueue(value);
	}

    public string GetContentFromBuffer()
    {
        return buffer.Dequeue();
    }
}
