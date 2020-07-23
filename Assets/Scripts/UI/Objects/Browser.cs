using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using PharoModule;
using LoggingModule;

public class Browser : InitializeBehaviour
{
    public PackageWindow package_list;
    public Transform class_list;
    public Transform method_list;
    public Transform classSideList;
    public Transform instanceSideList;
    public Toggle classSideToggle;
    public Toggle instanceSideToggle;
    public string lastSelectedSide = "InstanceSide";

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
            InteractionLogger.RegisterCodeDefinition("class", clean_code, responseString);
        }
        else
        {
            string currentPackage = package_list.getLastSelected().name;
            string currentClass = class_list.Find(currentPackage).gameObject
                .GetComponent<ClassWindow>().getLastSelected().name;

            string method_code = lastSelectedSide == "ClassSide" ?
                "(" + currentClass + " class) compile: '" + clean_code.Replace("'", "''") + "'" :
                currentClass + " compile: '" + clean_code.Replace("'", "''") + "'";

            // Getting method name
            string responseString = await Pharo.Execute(method_code);
            string methodName = Regex.Matches(clean_code, @"(\A(.*:?) )|(\A(.*:?)\n)")[0].Value;
            methodName = Regex.Replace(methodName, @"\n|\r|\t|\s", "");
            createOrUpdateMethod(currentPackage, currentClass, methodName, input_code);
            InteractionLogger.RegisterCodeDefinition("method", clean_code, responseString);
        }
    }

    void createOrUpdatePackage(string packageName)
    {
        // Getting package and its classes
        Transform existingPackage = package_list.transform.Find(packageName);
        BrowserPackage newPackage = !existingPackage ?
            Instantiator.Instance.PackageObject(package_list, packageName, field, null, this) :
            existingPackage.gameObject.GetComponent<BrowserPackage>();

        // Updating package
        if (!VRIDEController.sysData.data.ContainsKey(packageName))
            VRIDEController.sysData.data.Add(
                packageName, 
                new SortedDictionary<string, (string classCode,
                    List<(string methodName, string methodCode, string side)> classMethods)>());

        // Activating
        newPackage.click();
    }

    void createOrUpdateClass(string packageName, string className, string input_code)
    {
        // Getting class and its methods
        Transform package = class_list.Find(packageName);
        Transform existing_class = package.Find(className);
        BrowserClass new_class = !existing_class ?
            Instantiator.Instance.ClassObject(package.GetComponent<ClassWindow>(), className, field, 
                null, null, input_code, this) :
            existing_class.gameObject.GetComponent<BrowserClass>();

        //Updating class
        if (!VRIDEController.sysData.data[packageName].ContainsKey(className))
            VRIDEController.sysData.data[packageName].Add(
                className,
                (input_code,
                    new List<(string methodName, string methodCode, string side)>()));
        else
        {
            List<(string methodName, string methodCode, string side)> methods 
                = VRIDEController.sysData.data[packageName][className].classMethods;
            VRIDEController.sysData.data[packageName][className] = (input_code, methods);
        }

        // Activating
        new_class.sourceCode = input_code;
        new_class.click();

        LayoutRebuilder.ForceRebuildLayoutImmediate(package.gameObject.GetComponent<RectTransform>());
    }

    void createOrUpdateMethod(string packageName, string className, string methodName, string input_code)
    {
        // Getting methods
        string side = classSideToggle.isOn ? "ClassSide" : "InstanceSide";
        Transform classMethodList = method_list.Find(side + "/" + className);
        Transform existing_method = classMethodList.Find(methodName);
        BrowserMethod new_method = !existing_method ?
            Instantiator.Instance.MethodObject(classMethodList, className, methodName, field, input_code, this) :
            existing_method.gameObject.GetComponent<BrowserMethod>();

        // Updating method
        VRIDEController.sysData.data[packageName][className].classMethods.Remove((methodName, new_method.sourceCode, side));
        VRIDEController.sysData.data[packageName][className].classMethods.Add((methodName, input_code, side));

        // Activating
        new_method.sourceCode = input_code;
        new_method.click();

        LayoutRebuilder.ForceRebuildLayoutImmediate(classMethodList.gameObject.GetComponent<RectTransform>());
    }

    public void onSelectClassSide()
    {
        classSideToggle.isOn = true;
        instanceSideToggle.isOn = false;
        classSideList.gameObject.SetActive(true);
        instanceSideList.gameObject.SetActive(false);
        lastSelectedSide = "ClassSide";

        Color white, skyBlue;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out white) &&
            ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue))
        {
            var classSideColors = classSideToggle.colors;
            var instSideColors = instanceSideToggle.colors;

            classSideColors.normalColor = skyBlue;
            instSideColors.normalColor = white;

            classSideToggle.colors = classSideColors;
            instanceSideToggle.colors = instSideColors;
        }
    }

    public void onSelectInstanceSide()
    {
        classSideToggle.isOn = false;
        instanceSideToggle.isOn = true;
        classSideList.gameObject.SetActive(false);
        instanceSideList.gameObject.SetActive(true);
        lastSelectedSide = "InstanceSide";

        Color white, skyBlue;
        if (ColorUtility.TryParseHtmlString("#FFFFFF", out white) &&
            ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue))
        {
            var classSideColors = classSideToggle.colors;
            var instSideColors = instanceSideToggle.colors;

            classSideColors.normalColor = white;
            instSideColors.normalColor = skyBlue;

            classSideToggle.colors = classSideColors;
            instanceSideToggle.colors = instSideColors;
        }
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

    public override IEnumerator innerStart()
    {
        yield return base.innerStart();
        foreach (string key in VRIDEController.sysData.data.Keys)
        {
            yield return Instantiator.Instance.PackageObject(package_list, key, field, null, this);
        }
    }

    public override void onClose()
    {
        player.browsers.Remove(this);
        InteractionLogger.Discount("Browser");
        Destroy(gameObject);
    }

    public override void innerBehaviour()
    {
        if ((Input.anyKeyDown || Input.GetKeyUp(KeyCode.Backspace)) && field.isFocused)
        {
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f6 = Input.GetKeyDown(KeyCode.F6);
            bool s = Input.GetKeyDown("g");

            if (!(leftCmd || leftCtrl || f6 || s))
                onChangeInput();
            else if (((leftCmd || leftCtrl) && s) || f6)
                PharoDefine();
        }
    }
}
