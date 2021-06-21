using PharoModule;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using System.Text.RegularExpressions;
using UnityEngine.UI;
using System;
using System.Threading.Tasks;
using System.Collections;

public class ClassWindowCube : WindowCube
{
    [HideInInspector]
    public string className;
    [HideInInspector]
    public string parentName;
    [HideInInspector]
    public string[] instanceMethods;
    [HideInInspector]
    public string[] classMethods;
    [HideInInspector]
    public string[] instanceVars;
    [HideInInspector]
    public string[] classVars;
    [HideInInspector]
    public string[] incomingClasses;
    [HideInInspector]
    public string[] outgoingClasses;
    [HideInInspector]
    public string[] children;

    public AFrameLine aFrameLinePrefab;
    public ClassWindowCube classWindowCubePrefab;
    public CodeCubeMenu codeCubeMenuPrefab;
    public PharoVarCodeCube varCodeCubePrefab;
    public PharoMethodCodeCube methodCodeCubePrefab;
    public PharoScatterPlot scatterPlotPrefab;

    public TMP_InputField sourceCode;
    public TMP_InputField parentClassText;
    public TMP_InputField childClassesList;
    public TMP_InputField outgoingSendersList;
    public TMP_InputField incomeSendersList;
    public List<GameObject> loadingWheels;

    private bool loading;
    private ClassWindowCube parentClass;
    
    [HideInInspector]
    public List<ClassWindowCube> subClassesObjects;
    private List<PharoCodeCube> inspectObjects;
    private List<ClassWindowCube> incomeClassesObjects;
    private List<ClassWindowCube> outgoingClassesObjects;
    private PharoScatterPlot plot;

    public override void InnerStart()
    {
        base.InnerStart();
        subClassesObjects = new List<ClassWindowCube>();
        inspectObjects = new List<PharoCodeCube>();
        incomeClassesObjects = new List<ClassWindowCube>();
        outgoingClassesObjects = new List<ClassWindowCube>();
        UpdateData();

        if (!PharoScatterPlot.classes.Contains(className))
            PharoScatterPlot.classes.Add(className);
    }

    IEnumerator GenerateSubClasses()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();
                if (subClassesObjects.Count == 0)
                {
                    for (int i = 0; i < children.Length; i++)
                    {
                        ClassWindowCube childClass = CreateNewCube(children[i], new Vector3(i * 1.1f, -1.1f, 0f));
                        AddLine(childClass.gameObject, Color.blue);
                        subClassesObjects.Add(childClass);
                        yield return null;
                    }
                }
                else
                {
                    foreach (ClassWindowCube cube in subClassesObjects) 
                    {
                        Destroy(cube.gameObject);
                        yield return null;        
                    }
                    subClassesObjects = new List<ClassWindowCube>();
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
            yield return null;
        }
    }

    IEnumerator GenerateParentClass()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();
                if (parentClass == null && parentName != "Object")
                {
                    parentClass = CreateNewCube(parentName, new Vector3(0f, 1.1f, 0f));
                    parentClass.subClassesObjects = new List<ClassWindowCube>() { this };
                    AddLine(parentClass.gameObject, Color.black);
                }
                else
                {
                    Destroy(parentClass.gameObject);
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
            yield return null;
        }
    }

    IEnumerator GenerateIncomingClasses()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();

                if (incomeClassesObjects.Count == 0)
                {
                    for (int i = 0; i < incomingClasses.Length; i++)
                    {
                        string incomingClassName = incomingClasses[i].Split(new string[] { ">>" }, StringSplitOptions.None)[0];
                        ClassWindowCube incomingClass = CreateNewCube(incomingClassName, new Vector3(-(1f + i * 1.1f), 0f, 0f));
                        AddLine(incomingClass.gameObject, Color.red);
                        incomeClassesObjects.Add(incomingClass);
                        yield return null;
                    }
                }
                else
                {
                    foreach (ClassWindowCube ob in incomeClassesObjects) 
                    {
                        Destroy(ob.gameObject);
                        yield return null;
                    }
                    incomeClassesObjects = new List<ClassWindowCube>();
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
            yield return null;
        }
    }

    IEnumerator GenerateOutgoingClasses()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();

                if (outgoingClassesObjects.Count == 0)
                {
                    for (int i = 0; i < outgoingClasses.Length; i++)
                    {
                        ClassWindowCube outgoingClass = CreateNewCube(outgoingClasses[i], new Vector3((1f + i * 1.1f), 0f, 0f));
                        AddLine(outgoingClass.gameObject, Color.magenta);
                        outgoingClassesObjects.Add(outgoingClass);
                        yield return null;
                    }
                }
                else
                {
                    foreach (ClassWindowCube ob in outgoingClassesObjects) 
                    {
                        Destroy(ob.gameObject);
                        yield return null;
                    }
                    outgoingClassesObjects = new List<ClassWindowCube>();
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
            yield return null;
        }
    }

    IEnumerator GenerateInspectElements()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();
                if (inspectObjects.Count == 0)
                {
                    for (int i = 0; i < instanceVars.Length; i++)
                    {
                        if (!string.IsNullOrWhiteSpace(instanceVars[i]))
                        {
                            PharoVarCodeCube var = Instantiate(varCodeCubePrefab);
                            var.transform.localScale = new Vector3(.05f, .05f, .05f);
                            var.transform.position = transform.position;
                            var.isInstance = true;
                            var.varName = instanceVars[i];

                            AddLine(var.gameObject, Color.green);
                            MoveTo(var.transform, transform.position + (new Vector3(i * .1f, 0f, -.2f)), false);
                            inspectObjects.Add(var);
                            yield return null;
                        }
                    }

                    for (int i = 0; i < classVars.Length; i++)
                    {
                        if (!string.IsNullOrWhiteSpace(classVars[i]))
                        {
                            PharoVarCodeCube var = Instantiate(varCodeCubePrefab);
                            var.isInstance = false;
                            var.transform.position = transform.position;
                            var.transform.localScale = new Vector3(.05f, .05f, .05f);
                            var.varName = classVars[i];

                            AddLine(var.gameObject, Color.green);
                            MoveTo(var.transform, transform.position + (new Vector3(i * .1f, .1f, -.2f)), false);
                            inspectObjects.Add(var);
                            yield return null;
                        }
                    }

                    for (int i = 0; i < instanceMethods.Length; i++)
                    {
                        if (!string.IsNullOrWhiteSpace(instanceMethods[i]))
                        {
                            PharoMethodCodeCube m = Instantiate(methodCodeCubePrefab);
                            m.transform.position = transform.position;
                            m.transform.localScale = new Vector3(.05f, .05f, .05f);
                            m.methodName = instanceMethods[i];
                            m.isInstance = true;

                            /**
                            string methodCode = m.isInstance ?
                                "((" + className + " class)>>#" + m.methodName + ") sourceCode ." :
                                "(" + className + ">>#" + m.methodName + ") sourceCode .";

                            int linesOfCode = methodCode.Split('\n').Length;
                            m.transform.localScale = m.transform.localScale + new Vector3(linesOfCode, linesOfCode, linesOfCode) * .01f;
                            **/

                            AddLine(m.gameObject, Color.green);
                            MoveTo(m.transform, transform.position + (new Vector3(i * .1f, .2f, -.2f)), false);
                            inspectObjects.Add(m);
                            yield return null;
                        }
                    }

                    for (int i = 0; i < classMethods.Length; i++)
                    {
                        if (!string.IsNullOrWhiteSpace(classMethods[i]))
                        {
                            PharoMethodCodeCube m = Instantiate(methodCodeCubePrefab);
                            m.transform.position = transform.position;
                            m.transform.localScale = new Vector3(.05f, .05f, .05f);
                            m.methodName = classMethods[i];
                            m.isInstance = false;

                            /**
                            string methodCode = m.isInstance ?
                                "((" + className + " class)>>#" + m.methodName + ") sourceCode ." :
                                "(" + className + ">>#" + m.methodName + ") sourceCode .";

                            int linesOfCode = methodCode.Split('\n').Length;
                            m.transform.localScale = m.transform.localScale + new Vector3(linesOfCode, linesOfCode, linesOfCode) * .01f;
                            **/

                            AddLine(m.gameObject, Color.green);
                            MoveTo(m.transform, transform.position + (new Vector3(i * .1f, .3f, -.2f)), false);
                            inspectObjects.Add(m);
                            yield return null;
                        }
                    }
                }
                else
                {
                    foreach (PharoCodeCube ob in inspectObjects) 
                    {
                        Destroy(ob.gameObject);
                        yield return null;
                    }
                    inspectObjects = new List<PharoCodeCube>();
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
        }
    }

    private ClassWindowCube CreateNewCube(string cname, Vector3 localTargetPosition)
    {
        ClassWindowCube cube = Instantiator.Instance.ClassWindowCube();
        cube.transform.position = transform.position;
        cube.transform.forward = transform.forward;
        cube.className = cname;

        // Move Animation
        Vector3 globalTargetPosition = transform.TransformPoint(localTargetPosition);
        MoveTo(cube.transform, globalTargetPosition, false);

        // Connecto boht parent and child with a line
        //AddLine(cube.gameObject, Color.blue);

        return cube;
    }

    private void GenerateScatterPlot()
    {
        if (plot == null)
        {
            plot = Instantiate(scatterPlotPrefab, transform.Find("Cube (5)/FaceChildren"));
            plot.transform.localScale = new Vector3(.8f, .8f, 800f);
            plot.transform.localPosition = new Vector3(-.4f, -.4f, -450f);
            plot.GenerateCubes(className);
        }
        else
        {
            Destroy(plot.gameObject);
        }
    }

    private void ActivateLoadingWheels()
    {
        loading = true;
        foreach (GameObject ob in loadingWheels)
            ob.SetActive(true);
    }

    private void DeactivateLoadingWheels()
    {
        loading = false;
        foreach (GameObject ob in loadingWheels)
            ob.SetActive(false);
    }

    public AFrameLine AddLine(GameObject ob, Color c)
    {
        AFrameLine line = Instantiate(aFrameLinePrefab, ob.transform);
        line.GetComponent<Renderer>().material.color = c;
        line.startObject = gameObject;
        line.endObject = ob;
        return line;
    }

    public async void UpdateData()
    {
        ActivateLoadingWheels();

        string code = await Pharo.Execute(className + " definition .");
        string subclasses = await Pharo.Execute(className + " subclasses .");
        string incoming = await Pharo.Execute("SystemNavigation default allReferencesTo: " + className + " binding .");
        string outgoing = await Pharo.Execute(className + " dependentClasses .");
        string im = await Pharo.Execute(className + " methodDict keys asString .");
        string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");

        parentName = Regex.Match(code, @"([a-zA-Z0-9]+)\s+(subclass|variableByteSubclass):").Groups[1].Value;
        children = Regex.Replace(subclasses, @"an Array\(|\)|{|}|\.|#\(", "").Split(' ');
        instanceMethods = Regex.Replace(im, @"'|\(|\)|#|\n", "").Split(' ');
        classMethods = Regex.Replace(cm, @"'|\(|\)|#|\n", "").Split(' ');
        instanceVars = Regex.Match(code, @"instanceVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
        classVars = Regex.Match(code, @"classVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
        incomingClasses = Regex.Replace(incoming, @"an OrderedCollection\(|\)|\n", "").Split(' ');
        outgoingClasses = Regex.Replace(outgoing, @"an Array\(|\)|{|}|\.", "").Split(' ');

        sourceCode.text = code.Remove(0,1);
        parentClassText.text = "ParentClass:\n\n" + parentName;
        childClassesList.text = "Subclasses:\n\n" + string.Join("\n", children);
        incomeSendersList.text = "Incoming:\n\n" + string.Join("\n", incomingClasses);
        outgoingSendersList.text = "Outgoing:\n\n" + string.Join("\n", outgoingClasses);

        //int classSize = instanceMethods.Length + classMethods.Length + instanceVars.Length + classVars.Length;
        //transform.localScale = transform.localScale + (new Vector3(classSize, classSize, classSize)) * .001f;

        DeactivateLoadingWheels();
    }

    public override void OnActivate()
    {
        if (!loading)
        {
            Transform menu = transform.Find("CodeCubeMenu2(Clone)");
            if (menu)
                Destroy(menu.gameObject);
            else
            {
                CodeCubeMenu m = Instantiate(codeCubeMenuPrefab, transform);
                m.transform.forward = transform.forward;
                transform.Find("CodeCubeMenu2(Clone)/Panel/Income").GetComponent<Button>().onClick.AddListener(OnIncome);
                transform.Find("CodeCubeMenu2(Clone)/Panel/Inspect").GetComponent<Button>().onClick.AddListener(OnInspect);
                transform.Find("CodeCubeMenu2(Clone)/Panel/Outcome").GetComponent<Button>().onClick.AddListener(OnOutgoing);
                transform.Find("CodeCubeMenu2(Clone)/Panel (1)/Parent").GetComponent<Button>().onClick.AddListener(OnParent);
                transform.Find("CodeCubeMenu2(Clone)/Panel (1)/Children").GetComponent<Button>().onClick.AddListener(OnChildren);
                transform.Find("CodeCubeMenu2(Clone)/Panel (2)/Open").GetComponent<Button>().onClick.AddListener(OnOpen);
                transform.Find("CodeCubeMenu2(Clone)/Panel (2)/Close").GetComponent<Button>().onClick.AddListener(OnClose);
                transform.Find("CodeCubeMenu2(Clone)/Panel (2)/Destroy").GetComponent<Button>().onClick.AddListener(OnDestroyObject);
                transform.Find("CodeCubeMenu2(Clone)/Panel (2)/ScatterPlot").GetComponent<Button>().onClick.AddListener(OnGenerateScatterPlot);
            }
        }
    }

    public void OnIncome()
    {
        StartCoroutine(GenerateIncomingClasses());
    }

    public void OnInspect()
    {
        StartCoroutine(GenerateInspectElements());
    }

    public void OnOutgoing()
    {
        StartCoroutine(GenerateOutgoingClasses());
    }

    public void OnParent()
    {
        StartCoroutine(GenerateParentClass());
    }

    public void OnChildren()
    {
        StartCoroutine(GenerateSubClasses());
    }

    public void OnOpen()
    {
        Open();
    }

    public void OnClose()
    {
        Close();
    }

    public void OnGenerateScatterPlot()
    {
        GenerateScatterPlot();
    }

    public void OnDestroyObject()
    {
        Destroy(gameObject);
    }

    private void OnDestroy()
    {
        if (PharoScatterPlot.classes.Contains(className))
            PharoScatterPlot.classes.Remove(className);
    }
}
