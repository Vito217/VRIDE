using PharoModule;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using System.Text.RegularExpressions;
using UnityEngine.UI;
using System;

public class ClassWindowCube : WindowCube
{
    [HideInInspector]
    public string className;

    public AFrameLine aFrameLinePrefab;
    public ClassWindowCube classWindowCubePrefab;
    public CodeCubeMenu codeCubeMenuPrefab;
    public PharoVarCodeCube varCodeCubePrefab;
    public PharoMethodCodeCube methodCodeCubePrefab;

    public TMP_InputField sourceCode;
    public TMP_InputField parentClassText;
    public TMP_InputField childClassesList;
    public TMP_InputField outgoingSendersList;
    public TMP_InputField incomeSendersList;
    public List<GameObject> loadingWheels;

    private bool loading;
    private ClassWindowCube parentClass;
    
    [HideInInspector]
    public List<ClassWindowCube> subClasses;
    private List<GameObject> inspectElements;
    private List<ClassWindowCube> incomeClasses;
    private List<ClassWindowCube> outgoingClasses;

    public override void InnerStart()
    {
        base.InnerStart();
        subClasses = new List<ClassWindowCube>();
        inspectElements = new List<GameObject>();
        incomeClasses = new List<ClassWindowCube>();
        outgoingClasses = new List<ClassWindowCube>();
        UpdateData();
    }

    async void GenerateSubClasses()
    {
        // If  it isn't loading any other cubes
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();

                string code = await Pharo.Execute(className + " subclasses .");
                string[] sclasses = Regex.Replace(code, @"an Array\(|\)|{|}|\.|#\(", "").Split(' ');

                // Zero classes means its closed
                if (subClasses.Count == 0 && !code.Contains("#"))
                {
                    // Generate each subclass
                    for (int i = 0; i < sclasses.Length; i++)
                    {
                        ClassWindowCube childClass = CreateNewCube(
                            sclasses[i], new Vector3(i * 1.1f, -1.1f, 0f));

                        // Connecto boht parent and child with a line
                        AddLine(childClass.gameObject, Color.blue);

                        // Add to list
                        subClasses.Add(childClass);
                    }
                }
                else
                {
                    // Close by deleting all clases
                    foreach (ClassWindowCube cube in subClasses) Destroy(cube.gameObject);
                    subClasses = new List<ClassWindowCube>();
                }
            }
            finally
            {
                Destroy(transform.Find("CodeCubeMenu2(Clone)").gameObject);
                DeactivateLoadingWheels();
            }
        }
    }

    async void GenerateParentClass()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();
                string code = await Pharo.Execute(className + " definition .");
                string parentClassName = Regex.Match(code, @"([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;

                if (parentClass == null && parentClassName != "Object")
                {
                    // We create and assign the parent
                    parentClass = CreateNewCube(parentClassName, new Vector3(0f, 1.1f, 0f));
                    parentClass.subClasses = new List<ClassWindowCube>() { this };
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
            
        }
    }

    async void GenerateIncomingClasses()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();

                if (incomeClasses.Count == 0)
                {
                    string incoming = await Pharo.Execute("SystemNavigation default allReferencesTo: " + className + " binding .");
                    string[] incomingClasses = Regex.Replace(incoming, @"an OrderedCollection\(|\)|\n| class", "").Split(' ');

                    // Generate each subclass
                    for (int i = 0; i < incomingClasses.Length; i++)
                    {
                        string incomingClassName = incomingClasses[i].Split(new string[] { ">>" }, StringSplitOptions.None)[0];

                        Debug.Log(incomingClassName);

                        ClassWindowCube incomingClass = CreateNewCube(
                            incomingClassName, new Vector3(-(1f + i * 1.1f), 0f, 0f));

                        // Connecto boht parent and child with a line
                        AddLine(incomingClass.gameObject, Color.red);

                        // Add to list
                        incomeClasses.Add(incomingClass);
                    }
                }
                else
                {
                    foreach (ClassWindowCube ob in incomeClasses) Destroy(ob.gameObject);
                    incomeClasses = new List<ClassWindowCube>();
                }
            }
            finally
            {
                DeactivateLoadingWheels();
            }
        }
    }

    async void GenerateOutgoingClasses()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();

                if (outgoingClasses.Count == 0)
                {
                    string outgoing = await Pharo.Execute(className + " dependentClasses .");

                    Debug.Log(outgoing);

                    string[] outClasses = Regex.Replace(outgoing, @"an Array|\(|\)|\n|#|\[|\]|{|}|\.", "").Split(' ');

                    // Generate each subclass
                    for (int i = 0; i < outClasses.Length; i++)
                    {
                        ClassWindowCube outgoingClass = CreateNewCube(
                            outClasses[i], new Vector3((1f + i * 1.1f), 0f, 0f));

                        // Connecto boht parent and child with a line
                        AddLine(outgoingClass.gameObject, Color.magenta);

                        // Add to list
                        outgoingClasses.Add(outgoingClass);
                    }
                }
                else
                {
                    foreach (ClassWindowCube ob in outgoingClasses) Destroy(ob.gameObject);
                    outgoingClasses = new List<ClassWindowCube>();
                }
            }
            finally
            {
                DeactivateLoadingWheels();
            }
        }
    }

    async void GenerateInspectElements()
    {
        if (!loading)
        {
            try
            {
                ActivateLoadingWheels();
                if (inspectElements.Count == 0)
                {
                    string code = await Pharo.Execute(className + " definition .");
                    string im = await Pharo.Execute(className + " methodDict keys asString .");
                    string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");

                    string[] instanceMethods = Regex.Replace(im, @"'|\(|\)|#|\n", "").Split(' ');
                    string[] classMethods = Regex.Replace(cm, @"'|\(|\)|#|\n", "").Split(' ');
                    string[] instanceVars = Regex.Match(code, @"instanceVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
                    string[] classVars = Regex.Match(code, @"classVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');

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
                            inspectElements.Add(var.gameObject);
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
                            inspectElements.Add(var.gameObject);
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

                            string methodCode = m.isInstance ?
                                "((" + className + " class)>>#" + m.methodName + ") sourceCode ." :
                                "(" + className + ">>#" + m.methodName + ") sourceCode .";

                            int linesOfCode = methodCode.Split('\n').Length;
                            m.transform.localScale = m.transform.localScale + new Vector3(linesOfCode, linesOfCode, linesOfCode) * .01f;

                            AddLine(m.gameObject, Color.green);
                            MoveTo(m.transform, transform.position + (new Vector3(i * .1f, .2f, -.2f)), false);
                            inspectElements.Add(m.gameObject);
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


                            string methodCode = m.isInstance ?
                                "((" + className + " class)>>#" + m.methodName + ") sourceCode ." :
                                "(" + className + ">>#" + m.methodName + ") sourceCode .";

                            int linesOfCode = methodCode.Split('\n').Length;
                            m.transform.localScale = m.transform.localScale + new Vector3(linesOfCode, linesOfCode, linesOfCode) * .01f;

                            AddLine(m.gameObject, Color.green);
                            MoveTo(m.transform, transform.position + (new Vector3(i * .1f, .3f, -.2f)), false);
                            inspectElements.Add(m.gameObject);
                        }
                    }
                }
                else
                {

                    foreach (GameObject ob in inspectElements) Destroy(ob);
                    inspectElements = new List<GameObject>();
                }
            }
            finally
            {
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
        //cube.UpdateData();

        // Move Animation
        Vector3 globalTargetPosition = transform.TransformPoint(localTargetPosition);
        MoveTo(cube.transform, globalTargetPosition, false);

        // Connecto boht parent and child with a line
        AddLine(cube.gameObject, Color.blue);

        return cube;
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
        string parentClassName = Regex.Match(code, @"([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;
        string im = await Pharo.Execute(className + " methodDict keys asString .");
        string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");

        string[] instanceMethods = Regex.Replace(im, @"'|\(|\)|#|\n", "").Split(' ');
        string[] classMethods = Regex.Replace(cm, @"'|\(|\)|#|\n", "").Split(' ');
        string[] instanceVars = Regex.Match(code, @"instanceVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');
        string[] classVars = Regex.Match(code, @"classVariableNames:\s+'+([a-zA-Z0-9\s]+)'+").Groups[1].Value.Split(' ');


        sourceCode.text = code.Remove(0,1);
        parentClassText.text = "ParentClass:\n\n" + parentClassName;
        childClassesList.text = "Subclasses:\n\n" + Regex.Replace(subclasses, @"an Array\(|\)|{|}|\.|#\(", "").Replace(' ', '\n');
        incomeSendersList.text = "Incoming:\n\n" + Regex.Replace(incoming, @"an OrderedCollection\(|\)|\n", "").Replace(' ', '\n');
        outgoingSendersList.text = "Outgoing:\n\n" + Regex.Replace(outgoing, @"an Array\(|\)|{|}|\.", "").Replace(' ', '\n');

        int classSize = instanceMethods.Length + classMethods.Length + instanceVars.Length + classVars.Length;
        transform.localScale = transform.localScale + (new Vector3(classSize, classSize, classSize)) * .001f;

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
            }
        }
    }

    public void OnIncome()
    {
        GenerateIncomingClasses();
    }

    public void OnInspect()
    {
        GenerateInspectElements();
    }

    public void OnOutgoing()
    {
        GenerateOutgoingClasses();
    }

    public void OnParent()
    {
        GenerateParentClass();
    }

    public void OnChildren()
    {
        GenerateSubClasses();
    }

    public void OnOpen()
    {
        Open();
    }

    public void OnClose()
    {
        Close();
    }

    public void OnDestroyObject()
    {
        Destroy(gameObject);
    }
}
