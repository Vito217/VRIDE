using System.Linq;
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

    public override void innerBehaviour()
    {
        base.innerBehaviour();
        UpdateMetrics();
    }

    void UpdateMetrics()
    {
        metrics.text = "Number of Packages: " + package_list.transform.childCount + "\n" +
                       "Number of classes from selected package: " + class_list.transform.childCount + "\n" +
                       "Number of methods from selected class: " + methodList.transform.childCount + "\n" +
                       "Number of lines of code: " + field.text.Count(c => c == '\n');
    }
}
