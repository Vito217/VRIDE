using PharoModule;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using System.Text.RegularExpressions;
using UnityEngine.UI;

public class ClassWindowCube : WindowCube
{
    [HideInInspector]
    public string className;

    public AFrameLine aFrameLinePrefab;
    public ClassWindowCube classWindowCubePrefab;
    public CodeCubeMenu codeCubeMenuPrefab;

    public TMP_InputField sourceCode;
    public TMP_InputField childClassesList;
    public TMP_InputField outcomeSendersList;
    public TMP_InputField incomeSendersList;
    public List<GameObject> loadingWheels;

    private bool loading;
    private bool opened;
    private ClassWindowCube parentClass;
    private List<ClassWindowCube> subClasses;

    public override void InnerStart()
    {
        base.InnerStart();
        subClasses = new List<ClassWindowCube>();
    }

    async void GenerateSubClasses()
    {
        // If  it isn't loading any other cubes
        if (!loading)
        {
            // Zero classes means its closed
            if (subClasses.Count == 0)
            {
                ActivateLoadingWheels();
                string code = await Pharo.Execute(className + " subclasses .");
                string[] sclasses = Regex.Replace(code, @"an Array\(|\)|{|}|\.", "").Split(' ');

                // Generate each subclass
                for (int i = 0; i < sclasses.Length; i++)
                {
                    ClassWindowCube childClass = CreateNewCube(
                        sclasses[i], new Vector3(i, -1f, 0f));

                    // Connecto boht parent and child with a line
                    AddLine(childClass.gameObject, Color.blue);

                    // Add to list
                    subClasses.Add(childClass);
                }
                DeactivateLoadingWheels();
            }
            else
            {
                // Close by deleting all clases
                foreach (ClassWindowCube cube in subClasses) Destroy(cube.gameObject);
                subClasses = new List<ClassWindowCube>();
            }
        }
    }

    async void GenerateParentClass()
    {
        if (!loading)
        {
            if (parentClass == null)
            {
                ActivateLoadingWheels();
                string code = await Pharo.Execute(className + " definition .");
                string parentClassName = Regex.Match(code, @"([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;

                // We create and assign the parent
                parentClass = CreateNewCube(
                    parentClassName, new Vector3(0f, transform.localScale.y, 0f));

                DeactivateLoadingWheels();
            }
            else
            {
                Destroy(parentClass.gameObject);
            }
        }
    }

    async void GenerateInspectElements()
    {
        if (!loading)
        {
            ActivateLoadingWheels();
            string code = await Pharo.Execute(className + " definition .");
            string im = await Pharo.Execute(className + " methodDict keys asString .");
            string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");


            DeactivateLoadingWheels();
        }
    }

    private ClassWindowCube CreateNewCube(string cname, Vector3 localTargetPosition)
    {
        ClassWindowCube cube = Instantiator.Instance.ClassWindowCube();
        cube.transform.position = transform.position;
        cube.transform.forward = transform.forward;
        cube.className = cname;
        cube.UpdateData();

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
        string code = await Pharo.Execute(className + " definition .");
        string subclasses = await Pharo.Execute(className + " subclasses .");

        sourceCode.text = code;
        childClassesList.text = "Subclasses:\n\n" + Regex.Replace(subclasses, @"an Array\(|\)|{|}|\.", "").Replace(' ', '\n');
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
                transform.Find("CodeCubeMenu2(Clone)/Panel/Outcome").GetComponent<Button>().onClick.AddListener(OnOutcome);
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

    }

    public void OnInspect()
    {
        GenerateInspectElements();
    }

    public void OnOutcome()
    {

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
