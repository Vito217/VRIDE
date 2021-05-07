using PharoModule;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using UnityEngine;

public class PharoClassCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string className;
    [HideInInspector]
    public string packageName;
    [HideInInspector]
    public string parentClassName;
    [HideInInspector]
    public string[] instanceVars;
    [HideInInspector]
    public string[] instanceMethods;
    [HideInInspector]
    public string[] classVars;
    [HideInInspector]
    public string[] classMethods;
    [HideInInspector]
    public string sourceCode;

    public PharoClassCodeCube classCodeCubePrefab;
    public PharoVarCodeCube varCodeCubePrefab;
    public PharoMethodCodeCube methodCodeCubePrefab;
    public PharoPackageCodeCube packageCodeCubePrefab;

    public List<Transform> childLists;
    private bool opened = false;
    private bool firstOpen = true;
    private bool loading = false;

    public override void InnerStart()
    {
        base.InnerStart();
    }

    public void OnActivate()
    {
        if (!loading)
        {
            if (!opened)
            {
                Open();
            }
            else
            {
                Close();
            }
        }
    }

    async Task RetrieveInformation()
    {
        string code = await Pharo.Execute(className + " definition .");
        string im = await Pharo.Execute(className + " methodDict keys asString .");
        string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");

        sourceCode = code;
        instanceMethods = Regex.Replace(im, @"'|\(|\)|#|\n", "").Split(' ');
        classMethods = Regex.Replace(cm, @"'|\(|\)|#|\n", "").Split(' ');
        packageName = Regex.Match(code, @"package:\s+'+\s*([a-zA-Z0-9-]+)\s*'+").Groups[1].Value;
        instanceVars = Regex.Match(code, @"instanceVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
        classVars = Regex.Match(code, @"classVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
        parentClassName = Regex.Match(code, @"\A\s*([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;
    }

    void GenerateCubes()
    {
        foreach(string instanceVar in instanceVars)
        {
            PharoVarCodeCube var = Instantiate(varCodeCubePrefab, childLists[0]);
            var.varName = instanceVar;
        }

        foreach(string classVar in classVars)
        {
            PharoVarCodeCube var = Instantiate(varCodeCubePrefab, childLists[1]);
            var.varName = classVar;
        }

        foreach(string instanceMethod in instanceMethods)
        {
            PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[2]);
            method.methodName = instanceMethod;
        }

        foreach(string classMethod in classMethods)
        {
            PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[3]);
            method.methodName = classMethod;
        }

        PharoPackageCodeCube pack = Instantiate(packageCodeCubePrefab, childLists[4]); 
        pack.packageName = packageName;

        PharoClassCodeCube parent = Instantiate(classCodeCubePrefab, childLists[5]);
        parent.className = parentClassName;
    }

    async void Open()
    {
        if (firstOpen)
        {
            firstOpen = false;
            loading = true;
            StartCoroutine(LoadingAnimation());
            await RetrieveInformation();
            GenerateCubes();
            loading = false;
        }

        opened = true;
        RotateLocally(360);
        //ScaleTo(new Vector3(.4f, .4f, .4f));

        // INSTANCE VARS
        for (int i = 0; i < childLists[0].childCount; i++)
        {
            CodeCube child = childLists[0].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(i + 1, 0f, 0f));
        }

        // CLASS VARS
        for (int i = 0; i < childLists[1].childCount; i++)
        {
            CodeCube child = childLists[1].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, i + 1, 0f));
        }

        // INSTANCE METHODS
        for (int i = 0; i < childLists[2].childCount; i++)
        {
            CodeCube child = childLists[2].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, 0f, i + 1));
        }

        // CLASS METHODS
        for (int i = 0; i < childLists[3].childCount; i++)
        {
            CodeCube child = childLists[3].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(-(i + 1), 0f, 0f));
        }

        // PACKAGE
        for (int i = 0; i < childLists[4].childCount; i++)
        {
            CodeCube child = childLists[4].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, -(i + 1), 0f));
        }

        // SUPERCLASS
        for (int i = 0; i < childLists[5].childCount; i++)
        {
            CodeCube child = childLists[5].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, 0f, -(i + 1)));
        }
    }

    void Close()
    {
        opened = false;
        RotateLocally(-360);
        //ScaleTo(new Vector3(.5f, .5f, .5f));

        // INSTANCE VARS
        for (int i = 0; i < childLists[0].childCount; i++)
            childLists[0].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));

        // CLASS VARS
        for (int i = 0; i < childLists[1].childCount; i++)
            childLists[1].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));

        // INSTANCE METHODS
        for (int i = 0; i < childLists[2].childCount; i++)
            childLists[2].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));

        // CLASS METHODS
        for (int i = 0; i < childLists[3].childCount; i++)
            childLists[3].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));

        // PACKAGE
        for (int i = 0; i < childLists[4].childCount; i++)
            childLists[4].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));

        // SUPERCLASS
        for (int i = 0; i < childLists[5].childCount; i++)
            childLists[5].GetChild(i).GetComponent<CodeCube>().MoveLocallyTo(new Vector3(0f, 0f, 0f));
    }

    IEnumerator LoadingAnimation()
    {
        while (loading)
        {
            transform.Rotate(Time.deltaTime * rotationSpeed, 0f, 0f);
            yield return null;
        }
    }
}
