using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Xml.Linq;
using UnityEngine;

[System.Serializable]
public class Session
{
    public SystemData classesAndMethods = null;
    public List<BrowserData> browsers = new List<BrowserData>();
    public List<PlaygroundData> playgrounds = new List<PlaygroundData>();
    public List<InspectorData> inspectors = new List<InspectorData>();
    public List<SVGData> graphs = new List<SVGData>();

    public Session(
           SystemData data,
           List<BrowserData> brw,
           List<PlaygroundData> pg,
           List<InspectorData> ins,
           List<SVGData> grp
        )
    {
        classesAndMethods = data;
        browsers = brw;
        playgrounds = pg;
        inspectors = ins;
        graphs = grp;
    }
}