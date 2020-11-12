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
    public Toggle classSideToggle, instanceSideToggle;
    public Color white, skyBlue, gray;
    public TMP_InputField packageFilter;
    public TMP_InputField classFilter;
    public TMP_InputField methodFilter;
    public AutocompleteWordPicker wordPicker;
    private bool loadingPackages = true;

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
        keyboardTarget = data.selectedObject.GetComponent<TMP_InputField>();
        wordPicker.TextField = keyboardTarget;
        InteractionLogger.StartTimerFor("Browser");
    }

    public override void OnDeselect(BaseEventData data)
    {
        InteractionLogger.EndTimerFor("Browser");
    }

    public override void onClose()
    {
        if (loadingWheel == null || !loadingWheel.activeSelf)
        {
            SaveAndLoadModule.browsers.Remove(this);
            InteractionLogger.Discount("Browser");
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
            if (fromUIClick)
            {
                fromUIClick = false;
                keyboardTarget.caretPosition = lastCaretPosition;
                keyboardTarget.selectionAnchorPosition = lastAnchorPosition;
            }

            if (Input.anyKeyDown && !loadingWheel.activeSelf)
            {
                bool cmd = Input.GetKey(KeyCode.LeftCommand) ||
                           Input.GetKey(KeyCode.LeftControl) ||
                           Input.GetKey(KeyCode.RightControl);

                bool f6 = Input.GetKeyDown(KeyCode.F6);
                bool s = Input.GetKeyDown("s");

                //if (!(leftCmd || leftCtrl || f6 || s))
                //    onChangeInput();

                if ((cmd && s) || f6)
                    PharoDefine();
            }
            lastCaretPosition = keyboardTarget.caretPosition;
            lastAnchorPosition = keyboardTarget.selectionAnchorPosition;
        }
    }
}