using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using UnityEngine;
using PharoModule;

[System.Serializable]
public class SystemData
{
    public SortedDictionary<string, SortedDictionary<string, (string classCode, 
        List<(string methodName, string methodCode, string side)> classMethods)>> data =
       new SortedDictionary<string, SortedDictionary<string, (string classCode,
        List<(string methodName, string methodCode, string side)> classMethods)>>();

    public SystemData()
    {
        //retrieveSystemData();
    }

    public async Task LoadData(SortedDictionary<string, SortedDictionary<string, (string classCode,
        List<(string methodName, string methodCode, string side)> classMethods)>> newData)
    {
        await Task.Run(() => {
            foreach (KeyValuePair<string, SortedDictionary<string, (string classCode,
                List<(string methodName, string methodCode, string side)> classMethods)>> pair in newData)
            {
                if (!data.ContainsKey(pair.Key))
                    data.Add(pair.Key, pair.Value);
            }
        });
    }
    public async void retrieveSystemData()
    {
        string code = "RPackageOrganizer packageOrganizer packageNames .";
        string res = await Pharo.Execute(code);
        res = Regex.Replace(res, @"#\(#|'|\)|\n", "");
        string[] packages = res.Split(new string[] { " #" }, StringSplitOptions.None);
        foreach (string package in packages)
        {

            if (data.ContainsKey(package))
                continue;

            // New Package entry
            data.Add(package, new SortedDictionary<string, (string classCode,
                List<(string methodName, string methodCode, string side)> classMethods)>());

            code = "(RPackageOrganizer packageOrganizer packageNamed: '" + package + "') classes asString .";
            res = await Pharo.Execute(code);
            //Debug.Log(res);
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
                    data[package].Add(aClass, (classSourceCode,
                        new List<(string methodName, string methodCode, string side)>()));

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

                            data[package][aClass].classMethods.Add((
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

                            data[package][aClass].classMethods.Add((
                                classMethod, methodSourceCode, "ClassSide"));
                        }
                    }
                }
            }
        }
    }

    public SystemData(
        SortedDictionary<string, SortedDictionary<string, (string classCode,
        List<(string methodName, string methodCode, string side)> classMethods)>> d)
    {
        data = d;
    }
}