using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using System;

public class CodeCanvasClass : MonoBehaviour
{
    public TMP_InputField currentPackage;
    public TMP_InputField superClass;
    public TMP_InputField className;
    public List<TMP_InputField> instanceVarNames;
    public List<TMP_InputField> classVarNames;

    string sourceCode;

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        string s_className = className == null ? "" : className.text;
        string s_superClass = superClass == null ? "" : superClass.text;
        string s_currentPackage = currentPackage == null ? "" : currentPackage.text;

        string instVars = "'";
        for (int i = 0; i < instanceVarNames.Count && !String.IsNullOrWhiteSpace(instanceVarNames[i].text); i++)
        {
            if (i > 0) instVars += " ";
            instVars += instanceVarNames[i].text;
        }
        instVars += "'";

        string classVars = "'";
        for (int i = 0; i < classVarNames.Count && !String.IsNullOrWhiteSpace(classVarNames[i].text); i++)
        {
            if (i > 0) classVars += " ";
            classVars += classVarNames[i].text;
        }
        classVars += "'";

        sourceCode =
            s_superClass + "subclass: #" + s_className + "\n" +
                "\tinstanceVariableNames: " + instVars + "\n" +
                "\tclassVariableNames: " + classVars + "\n" +
                "\tpackage: '" + s_currentPackage + "'";
    }
}
