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
    public GameObject class_prefab;
    public GameObject method_prefab;
    public GameObject content_list_prefab;

    public GameObject class_list;
    public GameObject method_list;

    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) ||
                    Input.GetKey(KeyCode.LeftCommand) || Input.GetKey(KeyCode.RightCommand) ||
                        Input.GetMouseButton(0) || Input.GetMouseButton(1)))
                onChangeInput();
            else if ((Input.GetKey(KeyCode.LeftControl) || Input.GetKey(KeyCode.LeftCommand)) 
                        && Input.GetKeyDown("g"))
                PharoDefine();
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
            string current_class_name = class_list.GetComponent<ClassWindow>().getLastSelectedClass().name;
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
        Transform existing_class_transform = class_list.transform.Find(className);
        GameObject browser_class;

        if (!existing_class_transform)
        {
            // Create new browser class
            GameObject new_class = Instantiate(class_prefab);
            new_class.transform.SetParent(class_list.transform, false);
            new_class.GetComponent<TextMeshProUGUI>().text = className;
            new_class.name = className;

            // Create method list
            GameObject new_method_list = Instantiate(content_list_prefab);
            new_method_list.transform.SetParent(method_list.transform, false);
            new_method_list.name = className;
            new_method_list.SetActive(false);

            // Assign class variables
            BrowserClass new_class_component = new_class.GetComponent<BrowserClass>();
            new_class_component.name = className;
            new_class_component.sourceCode = input_code;
            new_class_component.field = GetComponent<TMP_InputField>();
            new_class_component.parent_window = new_class.transform.parent.gameObject;
            new_class_component.method_list = new_method_list;

            // Creating initialize method
            GameObject new_method = Instantiate(method_prefab);
            new_method.transform.SetParent(new_method_list.transform, false);
            new_method.GetComponent<TextMeshProUGUI>().text = "<i>template</i>";
            new_method.name = "template";

            //Assign method variables
            BrowserMethod new_method_component = new_method.GetComponent<BrowserMethod>();
            new_method_component.sourceCode = 
                "<b>messageSelectorAndArgumentNames</b>\n" +
                "    | temporary variable names |\n"+
                "    statements";
            new_method_component.field = GetComponent<TMP_InputField>();

            browser_class = new_class;
        }
        else
        {
            //Update class source code
            GameObject existing_class = existing_class_transform.gameObject;
            BrowserClass existing_component = existing_class.GetComponent<BrowserClass>();
            existing_component.sourceCode = input_code;

            browser_class = existing_class;
        }
        browser_class.GetComponent<Button>().onClick.Invoke();
    }

    void createOrUpdateMethod(string className, string methodName, string input_code)
    {
        Transform existing_method_transform = method_list.transform.Find(className).Find(methodName);

        if (!existing_method_transform)
        {
            GameObject new_method = Instantiate(method_prefab);
            new_method.transform.SetParent(method_list.transform.Find(className), false);
            new_method.GetComponent<TextMeshProUGUI>().text = methodName;
            new_method.name = methodName;

            //Assign method variables
            BrowserMethod new_method_component = new_method.GetComponent<BrowserMethod>();
            new_method_component.sourceCode = input_code;
            new_method_component.field = GetComponent<TMP_InputField>();
        }
        else
        {
            //Update class source code
            GameObject existing_method = existing_method_transform.gameObject;
            BrowserMethod existing_component = existing_method.GetComponent<BrowserMethod>();
            existing_component.sourceCode = input_code;
            field.text = existing_component.sourceCode;
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
}