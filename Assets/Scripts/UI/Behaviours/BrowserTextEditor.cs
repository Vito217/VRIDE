using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;
using PharoModule;
using LoggingModule;
using InstantiatorModule;

public class BrowserTextEditor : TextEditorBehaviour
{
    public PackageWindow package_list;
    public Transform class_list;
    public Transform method_list;

    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f6 = Input.GetKey(KeyCode.F6);
            bool s = Input.GetKey("g");

            if (!(leftCmd || leftCtrl || f6 || s))
                onChangeInput();
            else {
                if (((leftCmd || leftCtrl) && s) || f6)
                    PharoDefine();
            }
        }
    }

    async void PharoDefine()
    {
        // Cleaning code from RichText
        string input_code = field.text;
        string clean_code = cleanCode(input_code);

        if (clean_code.Contains("subclass"))
        {
            string responseString = await Pharo.Execute(clean_code);

            string packageName = Regex.Matches(clean_code, @"package:\s'(.*)'")[0].Groups[1].Value;
            string className = Regex.Matches(clean_code, @"\s#(.*)(\s|\n)")[0].Groups[1].Value;

            // Getting or updating package
            createOrUpdatePackage(packageName);
            createOrUpdateClass(packageName, className, input_code);
            foreach (GameObject browser in GameObject.FindGameObjectsWithTag("BrowserTextEditor"))
            {
                if (browser != gameObject)
                {
                    BrowserTextEditor b = browser.GetComponent<BrowserTextEditor>();
                    b.createOrUpdatePackage(packageName);
                    b.createOrUpdateClass(packageName, className, input_code);
                } 
            }
            InteractionLogger.RegisterCodeDefinition("class", clean_code, responseString);
        }
        else
        {
            string currentPackage = package_list.getLastSelectedPackage().name;
            string currentClass = class_list.Find(currentPackage).gameObject
                .GetComponent<ClassWindow>().getLastSelectedClass().name;
            string method_code = currentClass + " compile: '" + clean_code.Replace("'", "''") + "'";
            string responseString = await Pharo.Execute(method_code);

            // Getting method name
            string methodName = Regex.Matches(clean_code, @"(\A(.*:?) )|(\A(.*:?)\n)")[0].Value;
            methodName = Regex.Replace(methodName, @"\n|\r|\t|\s", "");
            createOrUpdateMethod(currentClass, methodName, input_code);
            foreach (GameObject browser in GameObject.FindGameObjectsWithTag("BrowserTextEditor"))
            {
                if (browser != gameObject)
                    browser.GetComponent<BrowserTextEditor>()
                        .createOrUpdateMethod(currentClass, methodName, input_code);
            }
            InteractionLogger.RegisterCodeDefinition("method", clean_code, responseString);
        }
    }

    void createOrUpdatePackage(string packageName)
    {
        // Getting package and its classes
        Transform existingPackage = package_list.transform.Find(packageName);
        BrowserPackage newPackage = !existingPackage ?
            Instantiator.PackageObject(package_list, packageName, field,
                Instantiator.ClassListObject(class_list, packageName, field)) :
            existingPackage.gameObject.GetComponent<BrowserPackage>();

        // Updating package
        if (!VRIDEController.data.classes.ContainsKey(packageName))
            VRIDEController.data.classes.Add(packageName, new List<Tuple<string, string>>());

        // Activating
        newPackage.click();
    }

    void createOrUpdateClass(string packageName, string className, string input_code)
    {
        // Getting class and its methods
        Transform package = class_list.Find(packageName);
        Transform existing_class = package.Find(className);
        BrowserClass new_class = !existing_class ?
            Instantiator.ClassObject(package.GetComponent<ClassWindow>(), className, field, 
                Instantiator.MethodListObject(method_list, className, field)) :
            existing_class.gameObject.GetComponent<BrowserClass>();

        //Updating class
        VRIDEController.data.classes[packageName].Remove(new Tuple<string, string>(className, new_class.sourceCode));
        VRIDEController.data.classes[packageName].Add(new Tuple<string, string>(className, input_code));

        if (!VRIDEController.data.methodLists.ContainsKey(className))
            VRIDEController.data.methodLists.Add(className, new List<Tuple<string, string>>());

        // Activating
        new_class.sourceCode = input_code;
        new_class.click();

        LayoutRebuilder.ForceRebuildLayoutImmediate(package.gameObject.GetComponent<RectTransform>());
    }

    void createOrUpdateMethod(string className, string methodName, string input_code)
    {
        // Getting methods
        Transform classMethodList = method_list.Find(className);
        Transform existing_method = classMethodList.Find(methodName);
        BrowserMethod new_method = !existing_method ?
            Instantiator.MethodObject(classMethodList, className, methodName, field) :
            existing_method.gameObject.GetComponent<BrowserMethod>();

        // Updating method
        VRIDEController.data.methodLists[className].Remove(new Tuple<string, string>(methodName, new_method.sourceCode));
        VRIDEController.data.methodLists[className].Add(new Tuple<string, string>(methodName, input_code));

        // Activating
        new_method.sourceCode = input_code;
        //new_method.click();
    }

    public override void onSelect()
    {
        base.onSelect();
        InteractionLogger.StartTimerFor("Browser");
    }

    public override void onDeselect()
    {
        base.onDeselect();
        InteractionLogger.EndTimerFor("Browser");
    }
}