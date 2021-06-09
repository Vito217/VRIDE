using PharoModule;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ClassWindowCube : WindowCube
{
    [HideInInspector]
    public string className;

    // Start is called before the first frame update
    public void OnPressInspection()
    {

    }

    public void OnPressParentClass()
    {

    }

    public void OnPressSubClasses()
    {

    }

    async void GenerateSubClasses()
    {

    }

    async void GenerateParentClass()
    {
        string code = await Pharo.Execute(className + " subclasses .");
    }

    async void GenerateInspectElements()
    {
        string code = await Pharo.Execute(className + " definition .");
        string im = await Pharo.Execute(className + " methodDict keys asString .");
        string cm = await Pharo.Execute("(" + className + " class) methodDict keys asString .");
    }
}
