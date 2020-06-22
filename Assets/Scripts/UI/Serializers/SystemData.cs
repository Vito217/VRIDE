using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class SystemData
{
    public List<Tuple<string, string>> classes = new List<Tuple<string, string>>();
    public Dictionary<string, List<Tuple<string, string>>> methodLists = 
        new Dictionary<string, List<Tuple<string, string>>>();

    public SystemData()
    {

    }

    public SystemData(List<Tuple<string, string>> cls,
                      Dictionary<string, List<Tuple<string, string>>> ml)
    {
        classes = cls;
        methodLists = ml;
    }
}
