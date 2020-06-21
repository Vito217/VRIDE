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
    public ClassWindow class_list;
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
            // Executing pharo code
            string responseString = await Pharo.Execute(clean_code);
            string className = Regex.Matches(clean_code, @"\s#(.*)(\s|\n)")[0].Groups[1].Value;
            createOrUpdateClass(className, input_code); 
            foreach(GameObject browser in GameObject.FindGameObjectsWithTag("BrowserTextEditor"))
            {
                if(browser != gameObject)
                    browser.GetComponent<BrowserTextEditor>()
                        .createOrUpdateClass(className, input_code);
            }
            InteractionLogger.RegisterCodeDefinition("class", clean_code, responseString);
        }
        else
        {
            // Executing pharo code
            string current_class_name = class_list.getLastSelectedClass().name;
            string method_code = current_class_name + " compile: '" + clean_code.Replace("'", "''") + "'";
            string responseString = await Pharo.Execute(method_code);

            // Getting method name
            string methodName = Regex.Matches(clean_code, @"(\A(.*:?) )|(\A(.*:?)\n)")[0].Value;
            methodName = Regex.Replace(methodName, @"\n|\r|\t|\s", "");
            createOrUpdateMethod(current_class_name, methodName, input_code);
            foreach (GameObject browser in GameObject.FindGameObjectsWithTag("BrowserTextEditor"))
            {
                if (browser != gameObject)
                    browser.GetComponent<BrowserTextEditor>()
                        .createOrUpdateMethod(current_class_name, methodName, input_code);
            }
            InteractionLogger.RegisterCodeDefinition("method", clean_code, responseString);
        }
    }

    void createOrUpdateClass(string className, string input_code)
    {
        Transform existing_class = class_list.transform.Find(className);
        BrowserClass new_class = !existing_class ?
            Instantiator.ClassObject(class_list, className, field, 
                Instantiator.MethodListObject(method_list, className, field)) :
            existing_class.gameObject.GetComponent<BrowserClass>();
        new_class.sourceCode = input_code;
        new_class.click();
    }

    void createOrUpdateMethod(string className, string methodName, string input_code)
    {
        Transform existing_method = method_list.transform.Find(className).Find(methodName);
        BrowserMethod new_method = !existing_method ?
            Instantiator.MethodObject(method_list, className, methodName, field) :
            existing_method.gameObject.GetComponent<BrowserMethod>();
        new_method.sourceCode = input_code;
        new_method.click();
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