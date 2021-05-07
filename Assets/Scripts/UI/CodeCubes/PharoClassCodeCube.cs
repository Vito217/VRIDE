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
    public AFrameLine aFrameLinePrefab;

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

            AddLine(var.gameObject, Color.green);
        }

        foreach(string classVar in classVars)
        {
            PharoVarCodeCube var = Instantiate(varCodeCubePrefab, childLists[1]);
            var.varName = classVar;

            AddLine(var.gameObject, Color.red);
        }

        foreach(string instanceMethod in instanceMethods)
        {
            PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[2]);
            method.methodName = instanceMethod;

            AddLine(method.gameObject, Color.blue);
        }

        foreach(string classMethod in classMethods)
        {
            PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[3]);
            method.methodName = classMethod;

            AddLine(method.gameObject, Color.yellow);
        }

        PharoPackageCodeCube pack = Instantiate(packageCodeCubePrefab, childLists[4]); 
        pack.packageName = packageName;

        AddLine(pack.gameObject, Color.cyan);

        PharoClassCodeCube parent = Instantiate(classCodeCubePrefab, childLists[5]);
        parent.className = parentClassName;

        AddLine(parent.gameObject, Color.magenta);
    }

    void MoveChildren()
    {
        // INSTANCE VARS
        for (int i = 0; i < childLists[0].childCount; i++)
        {
            CodeCube child = childLists[0].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(2 * (i + 1), 0f, 0f));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }

        // CLASS VARS
        for (int i = 0; i < childLists[1].childCount; i++)
        {
            CodeCube child = childLists[1].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, 2 * (i + 1), 0f));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }

        // INSTANCE METHODS
        for (int i = 0; i < childLists[2].childCount; i++)
        {
            CodeCube child = childLists[2].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, 0f, 2 * (i + 1)));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }

        // CLASS METHODS
        for (int i = 0; i < childLists[3].childCount; i++)
        {
            CodeCube child = childLists[3].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(-2 * (i + 1), 0f, 0f));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }

        // PACKAGE
        for (int i = 0; i < childLists[4].childCount; i++)
        {
            CodeCube child = childLists[4].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, -2 * (i + 1), 0f));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }

        // SUPERCLASS
        for (int i = 0; i < childLists[5].childCount; i++)
        {
            CodeCube child = childLists[5].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            child.MoveLocallyTo(new Vector3(0f, 0f, -2 * (i + 1)));
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }
    }

    void GetChildrenBack()
    {
        // INSTANCE VARS
        for (int i = 0; i < childLists[0].childCount; i++)
        {
            CodeCube child = childLists[0].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }

        // CLASS VARS
        for (int i = 0; i < childLists[1].childCount; i++)
        {
            CodeCube child = childLists[1].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }

        // INSTANCE METHODS
        for (int i = 0; i < childLists[2].childCount; i++)
        {
            CodeCube child = childLists[2].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }

        // CLASS METHODS
        for (int i = 0; i < childLists[3].childCount; i++)
        {
            CodeCube child = childLists[3].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }

        // PACKAGE
        for (int i = 0; i < childLists[4].childCount; i++)
        {
            CodeCube child = childLists[4].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }

        // SUPERCLASS
        for (int i = 0; i < childLists[5].childCount; i++)
        {
            CodeCube child = childLists[5].GetChild(i).GetComponent<CodeCube>();
            child.ScaleTo(child.transform.localScale * transform.localScale.x);
            child.MoveLocallyTo(new Vector3(0f, 0f, 0f));
        }
    }
    void CleanCubes()
    {
        foreach(Transform child in transform)
        {
            foreach(Transform cube in child)
            {
                Destroy(cube.gameObject);
            }
        }
    }

    async void Open()
    {
        loading = true;

        StartCoroutine(LoadingAnimation());
        await RetrieveInformation();

        GenerateCubes();
        RotateLocally(360);

        MoveChildren();

        loading = false;
        opened = true;
    }

    void Close()
    {
        opened = false;
        RotateLocally(-360);

        CleanCubes();
    }

    IEnumerator LoadingAnimation()
    {
        while (loading)
        {
            transform.Rotate(Time.deltaTime * rotationSpeed, 0f, 0f);
            yield return null;
        }
    }

    void AddLine(GameObject ob, Color c)
    {
        AFrameLine line = Instantiate(aFrameLinePrefab, transform.Find("Connecting Lines"));
        line.startObject = gameObject;
        line.endObject = ob;

        line.GetComponent<Renderer>().material.color = c;
    }
}
