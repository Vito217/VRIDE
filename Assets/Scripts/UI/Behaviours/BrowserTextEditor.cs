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

public class BrowserTextEditor : TextEditorBehaviour
{
    public BrowserClass class_prefab;
    public BrowserMethod method_prefab;

    public Transform content_list_prefab;
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
        // Check if class object exists
        BrowserClass new_class;
        Transform existing_class = class_list.transform.Find(className);
        if (!existing_class)
        {
            // Create new browser class
            new_class = Instantiate(class_prefab, class_list.transform, false);
            new_class.gameObject.GetComponent<TextMeshProUGUI>().text = className;
            new_class.gameObject.name = className;
            new_class.name = className;
            new_class.field = field;
            new_class.parent_window = class_list;

            // Create method list
            Transform new_method_list = Instantiate(content_list_prefab, method_list, false);
            new_method_list.Find("template").gameObject.GetComponent<BrowserMethod>().field = field;
            new_method_list.name = className;
            new_method_list.gameObject.SetActive(false);
            new_class.method_list = new_method_list.gameObject;
        }
        else
            new_class = existing_class.gameObject.GetComponent<BrowserClass>();
        new_class.sourceCode = input_code;
        new_class.gameObject.GetComponent<Button>().onClick.Invoke();
    }

    void createOrUpdateMethod(string className, string methodName, string input_code)
    {
        BrowserMethod new_method;
        Transform existing_method = method_list.transform.Find(className).Find(methodName);
        if (!existing_method)
        {
            new_method = Instantiate(method_prefab, method_list.Find(className), false);
            new_method.gameObject.GetComponent<TextMeshProUGUI>().text = methodName;
            new_method.gameObject.name = methodName;
            new_method.field = field;
        }
        else
            new_method = existing_method.gameObject.GetComponent<BrowserMethod>();
        new_method.sourceCode = input_code;
        new_method.gameObject.GetComponent<Button>().onClick.Invoke();
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