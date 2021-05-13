using System.Collections;
using System.Collections.Generic;
using System.Linq;
using TMPro;
using UnityEngine;

public class PlaygroundWindowCube : Playground
{
    public TMP_InputField metrics;

    void Start()
    {
        StartCoroutine(innerStart());
    }

    void Update()
    {
        innerBehaviour();
    }

    public override void innerBehaviour()
    {
        base.innerBehaviour();
        UpdateMetrics();
    }

    void UpdateMetrics()
    {
        metrics.text = "Number of lines of code: " + field.text.Count(c => c == '\n');
    }
}
