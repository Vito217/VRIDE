using System.Collections;
using System.Collections.Generic;
using PharoModule;
using UnityEngine;

public class PharoScatterPlot : MonoBehaviour
{
    public PharoClassCodeCube classCodeCubePrefab;
    public static List<string> classes = new List<string>();

    private Transform attatch;

    void Start()
    {
        //GenerateCubes();
    }

    void Update()
    {

    }

    public void Attatch(Transform ob)
    {
        attatch = ob;
    }

    public async void GenerateCubes(string currentClass)
    {
        float maxX = float.MinValue;
        float maxY = float.MinValue;
        float minX = float.MaxValue;
        float minY = float.MaxValue;

        List<(float, float, string)> data = new List<(float, float, string)>();

        foreach (string className in classes)
        {
            if (className != "Object" && className != "nil" && !className.Contains("#"))
            {
                string numberOfMethods = await Pharo.Execute(className + " numberOfMethods .");
                string numberOfLinesOfCode = await Pharo.Execute(className + " numberOfLinesOfCode .");

                float x = float.Parse(numberOfMethods);
                float y = float.Parse(numberOfLinesOfCode);

                maxX = Mathf.Max(maxX, x); minX = Mathf.Min(minX, x);
                maxY = Mathf.Max(maxY, y); minY = Mathf.Min(minY, y);

                data.Add((x, y, className));
            }
        }

        foreach ((float x, float y, string className) in data)
        {
            float newX = (x - minX) / (maxX - minX);
            float newY = (y - minY) / (maxY - minY);

            PharoClassCodeCube cube = Instantiate(classCodeCubePrefab, transform);
            cube.transform.localPosition = new Vector3(newX, newY, 0.5f);
            cube.className = className;

            if (currentClass == className)
                cube.GetComponent<Renderer>().material.color = Color.red;

            cube.LockInteraction();
        }
    }
}
