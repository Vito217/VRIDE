using PharoModule;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using System.Text.RegularExpressions;

public class ClassWindowCube : WindowCube
{
    [HideInInspector]
    public string className;

    public AFrameLine aFrameLinePrefab;
    public ClassWindowCube classWindowCubePrefab;
    public TMP_InputField sourceCode;
    public List<GameObject> loadingWheels;

    private bool loading;
    private ClassWindowCube parentClass;
    private List<ClassWindowCube> subClasses;

    public override void InnerStart()
    {
        base.InnerStart();
        subClasses = new List<ClassWindowCube>();
    }

    public void OnPressInspection()
    {

    }

    public void OnPressParentClass()
    {
        GenerateParentClass();
    }

    public void OnPressSubClasses()
    {
        GenerateSubClasses();
    }

    async void GenerateSubClasses()
    {
        if (!loading)
        {
            if(subClasses.Count == 0)
            {
                //ActivateLoadingWheels();
                string code = await Pharo.Execute(className + " subclasses .");
                string[] sclasses = Regex.Replace(code, @"an Array\(|\)|{|}|\.", "").Split(' ');
                
                for (int i = 0; i < sclasses.Length; i++)
                {
                    if (i == 0)
                    {
                        string subclass = sclasses[i];
                        ClassWindowCube childClass = Instantiator.Instance.ClassWindowCube();
                        childClass.className = subclass;
                        childClass.UpdateData();
                    }
                    
                    //childClass.className = subclass;
                    //subClasses.Add(childClass);
                    //MoveLocallyTo(childClass.transform, new Vector3(2 * (i + 1), 0f, 0f), false);
                    //AddLine(childClass.gameObject, Color.blue);
                }
                //DeactivateLoadingWheels();
            }
            else
            {
                //foreach (ClassWindowCube cube in subClasses)
                //    Destroy(cube.gameObject);
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
                string parentClass = Regex.Match(code, @"([a-zA-Z0-9]+)\s+subclass:").Groups[1].Value;

                GenerateParentClassCube(parentClass);


                sourceCode.text = code;
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

    private void GenerateParentClassCube(string className)
    {
        parentClass = Instantiate(classWindowCubePrefab);
        parentClass.className = className;
        MoveLocallyTo(parentClass.transform, transform.InverseTransformPoint(transform.position + new Vector3(0f, 1f, 0f)), false);
        AddLine(parentClass.gameObject, Color.blue);
    }

    public AFrameLine AddLine(GameObject ob, Color c)
    {
        AFrameLine line = Instantiate(aFrameLinePrefab);
        line.GetComponent<Renderer>().material.color = c;
        line.startObject = gameObject;
        line.endObject = ob;
        return line;
    }

    async void UpdateData()
    {
        string code = await Pharo.Execute(className + " definition .");
        sourceCode.text = code;
    }
}
