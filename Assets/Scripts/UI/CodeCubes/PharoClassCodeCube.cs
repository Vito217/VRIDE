using PharoModule;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TMPro;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.XR.Interaction.Toolkit;

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

    [HideInInspector]
    public string objectName;
    [HideInInspector]
    public List<string> classVarsTypes;
    [HideInInspector]
    public List<string> instVarsTypes;

    public PharoClassCodeCube classCodeCubePrefab;
    public PharoVarCodeCube varCodeCubePrefab;
    public PharoMethodCodeCube methodCodeCubePrefab;
    public PharoPackageCodeCube packageCodeCubePrefab;
    public CodeCubeText codeCubeTextPrefab;
    public AFrameLine aFrameLinePrefab;
    public GameObject codeCubeMenuPrefab;

    public List<Transform> childLists;

    private bool opened = false;
    private bool loading = false;
    private WindowCube browser;

    public void OnActivate()
    {
        if (!loading)
        {
            if (!opened)
            {
                Transform menu = transform.Find("CodeCubeMenu(Clone)");
                if (menu)
                    Destroy(menu.gameObject);
                else
                {
                    Instantiate(codeCubeMenuPrefab, transform);
                    transform.Find("CodeCubeMenu(Clone)/Panel/Button").GetComponent<Button>().onClick.AddListener(OnInspect);
                    transform.Find("CodeCubeMenu(Clone)/Panel/Button (1)").GetComponent<Button>().onClick.AddListener(OnDependences);
                    transform.Find("CodeCubeMenu(Clone)/Panel (2)/Button (1)").GetComponent<Button>().onClick.AddListener(OnBrowse);
                    transform.Find("CodeCubeMenu(Clone)/Panel (1)/Button (1)").GetComponent<Button>().onClick.AddListener(OnDestroyCube);
                }
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
        parentClassName = Regex.Match(code, @"([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;
    }

    void GenerateInspectionCubes()
    {
        int index = 0;
        foreach(string instanceVar in instanceVars)
        {
            if (!string.IsNullOrWhiteSpace(instanceVar))
            {
                PharoVarCodeCube var = Instantiate(varCodeCubePrefab, childLists[0]);
                var.GetComponent<Renderer>().material.color = Color.green;
                var.isInstance = true;
                var.varName = instanceVar;

                if (instVarsTypes != null && instVarsTypes.Count > 0)
                    var.classType = instVarsTypes[index];

                AddLine(var.gameObject, Color.green);
            }

            index++;
        }

        index = 0;
        foreach(string classVar in classVars)
        {
            if (!string.IsNullOrWhiteSpace(classVar))
            {
                PharoVarCodeCube var = Instantiate(varCodeCubePrefab, childLists[1]);
                var.GetComponent<Renderer>().material.color = Color.red;
                var.isInstance = false;
                var.varName = classVar;

                if (classVarsTypes != null && classVarsTypes.Count > 0)
                    var.classType = classVarsTypes[index];

                AddLine(var.gameObject, Color.red);
            }

            index++;
        }

        foreach(string instanceMethod in instanceMethods)
        {
            if (!string.IsNullOrWhiteSpace(instanceMethod))
            {
                PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[2]);
                method.GetComponent<Renderer>().material.color = Color.blue;
                method.isInstance = true;
                method.methodName = instanceMethod;

                AddLine(method.gameObject, Color.blue);
            }
        }

        foreach(string classMethod in classMethods)
        {
            if (!string.IsNullOrWhiteSpace(classMethod))
            {
                PharoMethodCodeCube method = Instantiate(methodCodeCubePrefab, childLists[3]);
                method.GetComponent<Renderer>().material.color = Color.yellow;
                method.isInstance = false;
                method.methodName = classMethod;

                AddLine(method.gameObject, Color.yellow);
            } 
        }
    }

    void GenerateDependencesCubes()
    {
        if (!string.IsNullOrWhiteSpace(packageName))
        {
            PharoPackageCodeCube pack = Instantiate(packageCodeCubePrefab, childLists[4]);
            pack.GetComponent<Renderer>().material.color = Color.cyan;
            pack.packageName = packageName;

            AddLine(pack.gameObject, Color.cyan);
        }

        if (!string.IsNullOrWhiteSpace(parentClassName))
        {
            PharoClassCodeCube parent = Instantiate(classCodeCubePrefab, childLists[5]);
            parent.GetComponent<Renderer>().material.color = Color.magenta;
            parent.className = parentClassName;

            AddLine(parent.gameObject, Color.magenta);
        }
    }

    void MoveInspectChildren()
    {
        // INSTANCE VARS
        for (int i = 0; i < childLists[0].childCount; i++)
        {
            CodeCube child = childLists[0].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(2 * (i + 1), 0f, 0f));
            child.ScaleTo(Vector3.one);
        }

        // CLASS VARS
        for (int i = 0; i < childLists[1].childCount; i++)
        {
            CodeCube child = childLists[1].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(0f, 2 * (i + 1), 0f));
            child.ScaleTo(Vector3.one);
        }

        // INSTANCE METHODS
        for (int i = 0; i < childLists[2].childCount; i++)
        {
            CodeCube child = childLists[2].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(0f, -2 * (i + 1), 0f));
            child.ScaleTo(Vector3.one);
        }

        // CLASS METHODS
        for (int i = 0; i < childLists[3].childCount; i++)
        {
            CodeCube child = childLists[3].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(-2 * (i + 1), 0f, 0f));
            child.ScaleTo(Vector3.one);
        }
    }

    void MoveDependencesChildren()
    {
        // PACKAGE
        for (int i = 0; i < childLists[4].childCount; i++)
        {
            CodeCube child = childLists[4].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(0f, 2 * (i + 1), 0f));
            child.ScaleTo(Vector3.one);
        }

        // SUPERCLASS
        for (int i = 0; i < childLists[5].childCount; i++)
        {
            CodeCube child = childLists[5].GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(-2 * (i + 1), 0f, 0f));
            child.ScaleTo(Vector3.one);
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

    async void Inspect()
    {
        loading = true;
        StartCoroutine(LoadingAnimation());
        await RetrieveInformation();
        GenerateInspectionCubes();
        RotateLocally(360);
        MoveInspectChildren();
        loading = false;
        opened = true;
    }

    async void Dependences()
    {
        loading = true;
        StartCoroutine(LoadingAnimation());
        await RetrieveInformation();
        GenerateDependencesCubes();
        RotateLocally(360);
        MoveDependencesChildren();
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

    public AFrameLine AddLine(GameObject ob, Color c)
    {
        AFrameLine line = Instantiate(aFrameLinePrefab, transform.Find("Connecting Lines"));
        line.startObject = gameObject;
        line.endObject = ob;

        line.GetComponent<Renderer>().material.color = c;

        return line;
    }

    public void OnHoverEnter(HoverEnterEventArgs args)
    {
        if (!isDragged)
        {
            Transform previousText = transform.Find("CodeCubeText(Clone)");
            if (previousText) Destroy(previousText.gameObject);

            CodeCubeText text = Instantiate(codeCubeTextPrefab, transform);
            text.GetComponent<TextMeshPro>().text = "class: " + className;

            if (!string.IsNullOrWhiteSpace(objectName))
                text.GetComponent<TextMeshPro>().text += "\nname: " + objectName;
        }
    }

    public void OnHoverExit(HoverExitEventArgs args)
    {
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }

    public void OnInspect()
    {
        Inspect();
        Destroy(transform.Find("CodeCubeMenu(Clone)").gameObject);
    }

    public void OnDependences()
    {
        Dependences();
        Destroy(transform.Find("CodeCubeMenu(Clone)").gameObject);
    }

    public void OnBrowse()
    {
        BrowserWindowCube wc = Instantiator.Instance.BrowserWindowCube();
        wc.transform.position = transform.position + transform.TransformPoint(1f, 0f, 0f);
        wc.transform.forward = transform.forward;

        BrowserPackage p = Instantiator.Instance.PackageObject(packageName, wc);
        BrowserClass c = Instantiator.Instance.ClassObject(className, wc);
        c.click();

        p.GetComponent<TextMeshProUGUI>().color = Color.white;
        c.GetComponent<TextMeshProUGUI>().color = Color.cyan;

        foreach (Transform child in wc.transform)
            LayoutRebuilder.ForceRebuildLayoutImmediate(child.GetComponent<RectTransform>());

        AFrameLine l = AddLine(wc.gameObject, Color.cyan);
        CodeCubeText t = Instantiate(codeCubeTextPrefab, l.transform);
        t.GetComponent<TextMeshPro>().text = "WindowCube";
    }

    public void OnDestroyCube()
    {
        Destroy(gameObject);
    }
}
