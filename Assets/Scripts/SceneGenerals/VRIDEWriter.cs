using System.IO;
using System.Text;

public class VRIDEWriter : StreamWriter
{
    StringBuilder buffer;

    public VRIDEWriter(Stream s) : base(s)
	{
        buffer = new StringBuilder();
	}

    public override void Write(string value)
    {
        buffer.Append(value);
	}

    public string GetContentFromBuffer()
    {
        string content = buffer.ToString();
        buffer.Clear();
        return content;
    }
}
