using System.Collections.Generic;

[System.Serializable]
public class Session
{
    public List<BrowserData> browsers = new List<BrowserData>();
    public List<PlaygroundData> playgrounds = new List<PlaygroundData>();
    public List<InspectorData> inspectors = new List<InspectorData>();
    public List<SVGData> graphs = new List<SVGData>();

    public Session(
           List<BrowserData> brw,
           List<PlaygroundData> pg,
           List<InspectorData> ins,
           List<SVGData> grp
        )
    {
        browsers = brw;
        playgrounds = pg;
        inspectors = ins;
        graphs = grp;
    }
}