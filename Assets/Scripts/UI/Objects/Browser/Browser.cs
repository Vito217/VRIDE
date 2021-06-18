using System;
using System.Collections;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using PharoModule;
using SaveAndLoad;
using LoggingModule;
using TMPro;

public class Browser : InitializeBehaviour
{
    public PackageWindow package_list;
    public ClassWindow class_list;
    public MethodWindow methodList;
    public SenderWindow senderList;
    public Toggle classSideToggle, instanceSideToggle;
    public PharoClassCodeCube pharoClassCodeCubePrefab;
    public ClassWindowCube pharoClassWindowCubePrefab;
    public CodeCubeText codeCubeTextPrefab;
    public TMP_InputField packageFilter;
    public TMP_InputField classFilter;
    public TMP_InputField methodFilter;
    public Button methodRemover;
    public Button classRemover;
    public Button packageRemover;
    public Button codeCubeLoader;
    public Button codeCubeLoader2;
    
    [HideInInspector]
    public Color white, skyBlue, gray;

    private bool loadingPackages = false;

    public async void PharoDefine()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            // Cleaning code from RichText
            string input_code = field.text;
            string clean_code = input_code;

            if (clean_code.Contains("subclass"))
            {
                //------------------------- CREATING A CLASS ------------------------------------------------------------------

                string className = Regex.Matches(clean_code, @"\s#([a-zA-Z]+)(\s|\n)")[0].Groups[1].Value;
                string packageName = Regex.Matches(clean_code, @"package:(\s*)('|""|'')([a-zA-Z\s-]+)('|""|'')")[0].Groups[3].Value;

                if (string.IsNullOrWhiteSpace(className) ||
                    string.IsNullOrWhiteSpace(packageName))
                    throw new Exception("Must specify a class and a package");

                if (className[0].ToString().ToUpper() != className[0].ToString())
                    throw new Exception("First character must be uppercase");

                clean_code = Regex.Replace(clean_code, @"(instanceVariableNames:)(\s*)('|""|'')([a-zA-Z\s]*)('|""|'')(\s|\n)", "$1$2'$4'$6");
                clean_code = Regex.Replace(clean_code, @"(classVariableNames:)(\s*)('|""|'')([a-zA-Z\s]*)('|""|'')(\s|\n)", "$1$2'$4'$6");
                clean_code = Regex.Replace(clean_code, @"(package:)(\s*)('|""|'')([a-zA-Z\s-]+)('|""|'')(\s|\n|\Z)", "$1$2'$4'$6");

                string responseString = await Pharo.Print(clean_code);
                if (responseString.Contains(className))
                {
                    //--------------------------- CRATING DEFAULT ACCESORS AND SETTERS -------------------------------

                    // Instance Variables
                    string[] iVars = Regex.Match(clean_code, @"instanceVariableNames:(\s*)'([a-zA-Z\s]*)'(\s|\n)").Groups[2].Value.Split(' ');
                    foreach(string var in iVars)
                    {
                        if (!String.IsNullOrWhiteSpace(var))
                        {
                            // Getter
                            string exists = await Pharo.Print(className + " canUnderstand: #" + var + " .");
                            if (exists.Contains("false"))
                                await Pharo.Print(className + " compile: '" + var + "\n\t ^ " + var + " .'");

                            // Setter
                            exists = await Pharo.Print(className + " canUnderstand: #" + var + ": .");
                            if (exists.Contains("false"))
                                await Pharo.Print(className + " compile: '" + var + ": aValue\n\t" + var + " := aValue .'");
                        }
                    }

                    // Class Variables
                    string[] cVars = Regex.Match(clean_code, @"classVariableNames:(\s*)'([a-zA-Z\s]*)'(\s|\n)").Groups[2].Value.Split(' ');
                    foreach (string var in cVars)
                    {
                        if (!String.IsNullOrWhiteSpace(var))
                        {
                            // Getter
                            string exists = await Pharo.Print("(" + className + " class) canUnderstand: #" + var + " .");
                            if (exists.Contains("false"))
                                await Pharo.Print("(" + className + " class) compile: '" + var + "\n\t ^ " + var + " .'");

                            // Setter
                            exists = await Pharo.Print("(" + className + " class) canUnderstand: #" + var + ": .");
                            if (exists.Contains("false"))
                                await Pharo.Print("(" + className + " class) compile: '" + var + ": aValue\n\t" + var + " := aValue .'");
                        }
                    }

                    // Getting or updating package
                    createOrUpdatePackage(packageName);
                    package_list.transform.Find(packageName).gameObject.GetComponent<BrowserPackage>().click();
                }
                else
                {
                    logText.text =
                        "<color=#C63737>"+responseString.Remove(responseString.LastIndexOf("\n"), 1)+"</color>";
                }
                InteractionLogger.RegisterCodeDefinition("class", clean_code, responseString);
            }
            else
            {
                //-------------------------- CREATING A METHOD ----------------------------------------------------------------

                string currentPackage = package_list.last_selected.name;
                string currentClass = class_list.last_selected.name;

                string method_code = classSideToggle.isOn ?
                    "(" + currentClass + " class) compile: '" + clean_code.Replace("'", "''") + "'" :
                    currentClass + " compile: '" + clean_code.Replace("'", "''") + "'";

                // Getting method name
                string responseString = await Pharo.Print(method_code);
                if (responseString.Contains("#"))
                {
                    class_list.last_selected.click();
                }
                else
                {
                    logText.text =
                        "<color=#C63737>"+responseString.Remove(responseString.LastIndexOf("\n"), 1)+"</color>";
                }
                InteractionLogger.RegisterCodeDefinition("method", clean_code, responseString);
            }
        }
        catch (Exception e)
        {
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        Reactivate();
    }

    void createOrUpdatePackage(string packageName)
    {
        Transform existingPackage = package_list.transform.Find(packageName);
        if (!existingPackage)
            Instantiator.Instance.PackageObject(packageName, this);
    }

    public void onSelectClassSide()
    {
        classSideToggle.isOn = true;
        instanceSideToggle.isOn = false;

        var classSideColors = classSideToggle.colors;
        var instSideColors = instanceSideToggle.colors;
        classSideColors.normalColor = skyBlue;
        instSideColors.normalColor = white;
        classSideToggle.colors = classSideColors;
        instanceSideToggle.colors = instSideColors;

        if (methodList.gameObject.activeSelf)
            methodList.Load();
    }

    public void onSelectInstanceSide()
    {
        classSideToggle.isOn = false;
        instanceSideToggle.isOn = true;

        var classSideColors = classSideToggle.colors;
        var instSideColors = instanceSideToggle.colors;
        classSideColors.normalColor = white;
        instSideColors.normalColor = skyBlue;
        classSideToggle.colors = classSideColors;
        instanceSideToggle.colors = instSideColors;

        if (methodList.gameObject.activeSelf)
            methodList.Load();
    }

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("Browser", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        InteractionLogger.EndTimerFor("Browser", GetInstanceID().ToString());
    }

    public override void onClose()
    {
        if (!SomethingIsLoading())
        {
            SaveAndLoadModule.browsers.Remove(this);
            InteractionLogger.Discount("Browser", GetInstanceID().ToString());
            Destroy(gameObject);
        }
    }

    public override IEnumerator innerStart()
    {
        ColorUtility.TryParseHtmlString("#FFFFFF", out white);
        yield return null;
        ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue);
        yield return null;
        ColorUtility.TryParseHtmlString("#9D9D9D", out gray);
        yield return null;
    }

    public override void innerBehaviour()
    {
        if (loadingPackages)
        {
            loadingPackages = false;
            package_list.Load();
        }
        else if (keyboardTarget.isFocused)
        {
            if (Input.anyKeyDown && !SomethingIsLoading())
            {
                bool cmd = Input.GetKey(KeyCode.LeftCommand) ||
                           Input.GetKey(KeyCode.LeftControl) ||
                           Input.GetKey(KeyCode.RightControl);

                bool f6 = Input.GetKeyDown(KeyCode.F6);
                bool s = Input.GetKeyDown("s");

                if ((cmd && s) || f6)
                    PharoDefine();
            }
        }
    }

    public void LoadCodeCube()
    {
        PharoClassCodeCube classCodeCube = Instantiate(pharoClassCodeCubePrefab);
        classCodeCube.className = class_list.last_selected.name;
        classCodeCube.packageName = package_list.last_selected.name;

        float width = GetComponent<RectTransform>().sizeDelta.x *
                    transform.Find("Panel").GetComponent<RectTransform>().localScale.x;

        classCodeCube.transform.position = transform.TransformPoint(width, 0f, 0f);
        classCodeCube.transform.forward = transform.forward;
    }

    public void LoadClassWindowCube()
    {
        DeactivateTemporarily();
        ClassWindowCube classCodeCube = Instantiate(pharoClassWindowCubePrefab);
        classCodeCube.className = class_list.last_selected.name;
        classCodeCube.sourceCode.text = field.text;
        classCodeCube.UpdateData();

        float width = GetComponent<RectTransform>().sizeDelta.x *
                    transform.Find("Panel").GetComponent<RectTransform>().localScale.x;

        classCodeCube.transform.position = transform.TransformPoint(width, 0f, 0f);
        classCodeCube.transform.forward = transform.forward;
        Reactivate();
    }

    void LateUpdate()
    {
        CountLines();
        HighlightCode();
    }

    public void LoadPackages()
    {
        codeCubeLoader.interactable = false;
        loadingPackages = true;
    }

    public void RemoveLastSelectedPackage()
    {
        codeCubeLoader.interactable = false;
        string packageName = package_list.last_selected.name;
        DeletePackage(packageName);
    }

    public void RemoveLastSelectedClass()
    {
        codeCubeLoader.interactable = false;
        string className = class_list.last_selected.name;
        DeleteClass(className);
    }

    public void RemoveLastSelectedMethod()
    {
        string methodName = methodList.last_selected.name;
        string className = class_list.last_selected.name;
        DeleteMethod(className, methodName);
    }

    async void DeleteMethod(string className, string methodName)
    {
        await Pharo.Print("(" + className + " >> #" + methodName + ") removeFromSystem .");
        methodRemover.interactable = false;
        methodList.Load();
    }

    async void DeleteClass(string className)
    {
        await Pharo.Print(className + " removeFromSystem .");
        methodList.FullClean();
        methodFilter.interactable = false;
        classRemover.interactable = false;
        class_list.Load();
    }

    async void DeletePackage(string packageName)
    {
        await Pharo.Print("(RPackageOrganizer packageOrganizer packageNamed: '" + packageName + "') removeFromSystem .");
        class_list.FullClean();
        classFilter.interactable = false;
        packageRemover.interactable = false;
        package_list.Load();
    }

    void HighlightCode()
    {
        TMP_TextInfo textInfo = field.textComponent.textInfo;

        foreach(TMP_WordInfo wordInfo in textInfo.wordInfo) 
            PaintWord(textInfo, wordInfo, Color.white);

        for(int i = 0; i < textInfo.wordInfo.Length; i++)
        {
            try
            {
                TMP_WordInfo wordInfo = textInfo.wordInfo[i];
                if (Regex.Match(field.text, @"(subclass|variableByteSubclass):[\s]+#").Success)
                {
                    if (i == 0)
                        PaintWord(textInfo, wordInfo, Color.cyan);
                    else if (i == 2)
                        PaintWord(textInfo, wordInfo, Color.green);
                }
                else
                    if (i == 0)
                        PaintWord(textInfo, wordInfo, Color.magenta);
            }
            catch
            {
                continue;
            }
        }
        field.textComponent.UpdateVertexData(TMP_VertexDataUpdateFlags.All);
    }
}