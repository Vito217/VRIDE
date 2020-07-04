using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using PharoModule;

[System.Serializable]
public class SystemData
{
    //                Package            Class          Def               Method    SC     Side
    public Dictionary<string, Dictionary<string, Tuple<string, List<Tuple<string, string, string>>>>> data =
        new Dictionary<string, Dictionary<string, Tuple<string, List<Tuple<string, string, string>>>>>();

    public SystemData()
    {
        retrieveSystemData();
    }

    public async void retrieveSystemData()
    {
        string code = "RPackageOrganizer packageOrganizer packageNames .";
        string res = await Pharo.Execute(code);
        res = Regex.Replace(res, @"#\(#|'|\)|\n", "");
        string[] packages = res.Split(new string[] { " #" }, StringSplitOptions.None);
        foreach(string package in packages)
        {

            if (data.ContainsKey(package))
                continue;

            // New Package entry
            data.Add(package, new Dictionary<string, Tuple<string, List<Tuple<string, string, string>>>>());

            code = "(RPackageOrganizer packageOrganizer packageNamed: '" + package + "') classes asString .";
            res = await Pharo.Execute(code);
            Debug.Log(res);
            res = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "");
            string[] classesList = res.Split(' ');
            if (classesList.Length > 0 && classesList[0] != "")
            {
                foreach (string aClass in classesList)
                {
                    Debug.Log(package + " | " + aClass);

                    if (data[package].ContainsKey(aClass) || aClass == "class")
                        continue;

                    string classSourceCode = await Pharo.Execute(aClass + " definition .");
                    classSourceCode = classSourceCode.Remove(0, 1);
                    classSourceCode = classSourceCode.Remove(classSourceCode.Length - 1, 1);

                    // New Class Entry
                    data[package].Add(aClass, new Tuple<string, List<Tuple<string, string, string>>>(
                        classSourceCode, new List<Tuple<string, string, string>>()));

                    string instanceSideCode = aClass + " methodDict keys asString .";
                    string classSideCode = "(" + aClass + " class) methodDict keys asString .";
                    string instanceRes = await Pharo.Execute(instanceSideCode);
                    string classRes = await Pharo.Execute(classSideCode);

                    string[] instSideMethods = Regex.Replace(instanceRes, @"'|\(|\)|#|\n", "").Split(' ');
                    if (instSideMethods.Length > 0 && instSideMethods[0] != "")
                    {
                        foreach (string instanceMethod in instSideMethods)
                        {
                            string methodSourceCode = await Pharo.Execute(
                                "(" + aClass + ">>#" + instanceMethod + ") sourceCode ."
                            );
                            methodSourceCode = methodSourceCode.Remove(0, 1);
                            methodSourceCode = methodSourceCode.Remove(methodSourceCode.Length - 1, 1);

                            data[package][aClass].Item2.Add(new Tuple<string, string, string>(
                                instanceMethod, methodSourceCode, "InstanceSide"));
                        }
                    }
                    string[] classSideMethods = Regex.Replace(classRes, @"'|\(|\)|#|\n", "").Split(' ');
                    if (classSideMethods.Length > 0 && classSideMethods[0] != "")
                    {
                        foreach (string classMethod in classSideMethods)
                        {
                            string methodSourceCode = await Pharo.Execute(
                                "((" + aClass + " class)>>#" + classMethod + ") sourceCode ."
                            );
                            methodSourceCode = methodSourceCode.Remove(0, 1);
                            methodSourceCode = methodSourceCode.Remove(methodSourceCode.Length - 1, 1);

                            data[package][aClass].Item2.Add(new Tuple<string, string, string>(
                                classMethod, methodSourceCode, "ClassSide"));
                        }
                    }
                }
            }
        }
    }

    public SystemData(
        Dictionary<string, Dictionary<string, Tuple<string, List<Tuple<string, string, string>>>>> d)
    {
        data = d;
    }
}
