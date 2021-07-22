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

    public override PharoClassCodeCube InstantiateCube()
    {
        PharoClassCodeCube cube = Instantiate(codeCubePrefab);
        cube.transform.position = transform.TransformPoint(1f, 0f, -1f);
        cube.transform.forward = transform.forward;

        return cube;
    }

    public override Inspector GenerateInspector()
    {
        Inspector insp = Instantiator.Instance.Inspector();
        insp.transform.position = transform.TransformPoint(1f, 0f, -1f);
        insp.transform.forward = transform.forward;

        return insp;
    }
}
