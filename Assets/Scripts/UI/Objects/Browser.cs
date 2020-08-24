﻿using System;
using System.Collections;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using PharoModule;
using SaveAndLoad;
using LoggingModule;

public class Browser : InitializeBehaviour
{
    public PackageWindow package_list;
    public ClassWindow class_list;
    public MethodWindow methodList;
    public Toggle classSideToggle, instanceSideToggle;
    public Color white, skyBlue;
    private bool loadingPackages = true;

    async void PharoDefine()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            // Cleaning code from RichText
            string input_code = field.text;
            string clean_code = cleanCode(input_code);

            if (clean_code.Contains("subclass"))
            {
                string className = Regex.Matches(clean_code, @"\s#(.*)(\s|\n)")[0].Groups[1].Value;
                string packageName = Regex.Matches(clean_code, @"package:\s*'(.*)'")[0].Groups[1].Value;
                
                if (string.IsNullOrWhiteSpace(className) ||
                    string.IsNullOrWhiteSpace(packageName))
                    throw new Exception("Must specify a class and a package");

                if (className[0].ToString().ToUpper() != className[0].ToString())
                    throw new Exception("First character must be uppercase");

                string responseString = await Pharo.Print(clean_code);
                if (responseString.Contains(className))
                {
                    // Getting or updating package
                    createOrUpdatePackage(packageName);
                    package_list.transform.Find(packageName).gameObject.GetComponent<BrowserPackage>().click();
                }
                else
                {
                    output = " -> " + responseString.Remove(responseString.LastIndexOf("\n"), 1);
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
                if (responseString.Contains("#")) class_list.last_selected.click();
                else output = " -> " + responseString.Remove(responseString.LastIndexOf("\n"), 1);
                InteractionLogger.RegisterCodeDefinition("method", clean_code, responseString);
            }
        }
        catch (Exception e)
        {
            output = " -> [Error] " + e.Message;
        }
        finally
        {
            field.text += output;
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

    public override void onClose()
    {
        SaveAndLoadModule.browsers.Remove(this);
        InteractionLogger.Discount("Browser");
        Destroy(gameObject);
    }

    public override IEnumerator innerStart()
    {
        ColorUtility.TryParseHtmlString("#FFFFFF", out white);
        yield return null;
        ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue);
        yield return base.innerStart();
    }

    public override void innerBehaviour()
    {
        if (loadingPackages)
        {
            loadingPackages = false;
            //DeactivateTemporarily();
            package_list.Load();
        }
        else if (Input.anyKeyDown && field.isFocused && !loadingWheel.activeSelf)
        {
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f6 = Input.GetKeyDown(KeyCode.F6);
            bool s = Input.GetKeyDown("s");

            if (!(leftCmd || leftCtrl || f6 || s))
                onChangeInput();
            else if (((leftCmd || leftCtrl) && s) || f6)
                PharoDefine();
        }
    }
}
