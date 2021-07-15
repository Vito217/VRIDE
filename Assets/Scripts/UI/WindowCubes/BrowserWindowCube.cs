using System.Collections;
using System.Linq;
using System.Text.RegularExpressions;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class BrowserWindowCube : Browser
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

    public override IEnumerator innerStart()
    {
        return base.innerStart();
    }

    public override void innerBehaviour()
    {
        base.innerBehaviour();
        UpdateMetrics();
    }

    void UpdateMetrics()
    {
        metrics.text = "Number of Packages: " + package_list.transform.childCount + "\n" +
                       "Number of classes from selected package: " + (class_list.transform.childCount - 1) + "\n" +
                       "Number of methods from selected class: " + (methodList.transform.childCount - 1) + "\n" +
                       "Number of lines of code: " + (field.text.Count(c => c == '\n') + 1) + "\n";

        if(class_list.last_selected != null)
        {
            int instanceVars = Regex.Match(field.text, @"instanceVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ').Length;
            int classVars = Regex.Match(field.text, @"classVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ').Length;

            metrics.text += "Number of Inst. Vars: " + instanceVars + "\n" +
                            "Number of Class. Vars: " + classVars + "\n";
        }
    }

    public new void LoadCodeCube()
    {
        PharoClassCodeCube classCodeCube = Instantiate(pharoClassCodeCubePrefab);
        classCodeCube.className = class_list.last_selected.name;
        classCodeCube.packageName = package_list.last_selected.name;
        classCodeCube.transform.position = transform.TransformPoint(1f, 0f, -1f);
        classCodeCube.transform.forward = transform.forward;
    }

    public new void LoadClassWindowCube()
    {
        DeactivateTemporarily();
        ClassWindowCube classCodeCube = Instantiate(pharoClassWindowCubePrefab);
        classCodeCube.className = class_list.last_selected.name;
        classCodeCube.sourceCode.text = field.text;
        classCodeCube.UpdateData();
        classCodeCube.transform.position = transform.TransformPoint(1f, 0f, -1f);
        classCodeCube.transform.forward = transform.forward;
        Reactivate();
    }
}
