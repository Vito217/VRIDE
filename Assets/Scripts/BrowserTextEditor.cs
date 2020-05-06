using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

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
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetMouseButton(0) || Input.GetMouseButton(1)))
                onChangeInput();
            else if (Input.GetKey(KeyCode.LeftControl) && Input.GetKeyDown("g"))
                PharoDefine();
        }
    }

    async void PharoDefine()
    {
        // Cleaning code from RichText
        string input_code = field.text;
        string clean_code = input_code;
        clean_code = clean_code.Replace("<color=#b32d00>", "");
        clean_code = clean_code.Replace("<color=#00ffffff>", "");
        clean_code = clean_code.Replace("</color>", "");
        clean_code = clean_code.Replace("<b>", "");
        clean_code = clean_code.Replace("</b>", "");
        clean_code = clean_code.Replace("<br>", " ");

        if (clean_code.Contains("subclass"))
        {
            // Steps:
            // 1- Execute Pharo code
            // 2- Check if Class exists in Unity's Browser
            // 3- If not:
            //      - Create new BrowserClass with className. Make it child of Browser/Classes/..../content
            //      - Create new non-active method list (ScrollableWindowContent) with className. Make it child of Browser/Methdos/..../content
            //      - Assign name, source_code, text_field, parent_window and method_list to new class object
            //      - Create a initialize method (BrowserMethod). Make it child of Browser/Methdos/..../content/<classname>
            //      - Assign name, source_code and text_field to new method
            // 4- Else:
            //      - Update class' source code

            // Executing pharo code
            var content = new StringContent(clean_code, Encoding.UTF8);
            var response = await client.PostAsync(IP, content);
            var responseString = await response.Content.ReadAsStringAsync();
            //Debug.Log(responseString);

            // Getting class name
            Regex rgx = new Regex(@"\s#(.*)(\s|\n)");
            string className = rgx.Matches(clean_code)[0].Groups[1].Value;

            // Check if class object exists
            Transform existing_class_transform = class_list.transform.Find(className);

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
                new_class_component.field = this.gameObject.GetComponent<TMP_InputField>();
                new_class_component.parent_window = new_class.transform.parent.gameObject;
                new_class_component.method_list = new_method_list;

                // Creating initialize method
                GameObject new_method = Instantiate(method_prefab);
                new_method.transform.SetParent(new_method_list.transform, false);
                new_method.GetComponent<TextMeshProUGUI>().text = "initialize";
                new_method.name = "initialize";
                string method_code = className + " compile: 'initialize super initialize .'";
                var res = await client.PostAsync(IP, new StringContent(method_code, Encoding.UTF8));

                //Assign method variables
                BrowserMethod new_method_component = new_method.GetComponent<BrowserMethod>();
                new_method_component.sourceCode = "initialize\n\tsuper initialize .";
                new_method_component.field = this.gameObject.GetComponent<TMP_InputField>();

                class_list.GetComponent<ClassWindow>().setLastSelectedClass(new_class_component);
            }
            else
            {
                //Update class source code
                GameObject existing_class = existing_class_transform.gameObject;
                BrowserClass existing_component = existing_class.GetComponent<BrowserClass>();
                existing_component.sourceCode = input_code;
            }

        }
        else
        {
            GameObject class_window = class_list;
            BrowserClass current_class = class_window.GetComponent<ClassWindow>().getLastSelectedClass();
            string current_class_name = current_class.name;

            // Executing pharo code
            clean_code = clean_code.Replace("'", "''");
            string method_code = current_class_name + " compile: '" + clean_code + "'";
            var response = await client.PostAsync(IP, new StringContent(method_code, Encoding.UTF8));
            var responseString = await response.Content.ReadAsStringAsync();

            // Getting method name
            Regex rgx = new Regex(@"(\A(.*:?) )|(\A(.*:?)\n)");
            string methodName = rgx.Matches(clean_code)[0].Value;
            methodName = methodName.Replace("\n", "");
            methodName = methodName.Replace("\r", "");
            methodName = methodName.Replace("\t", "");
            methodName = methodName.Replace(" ", "");

            Transform existing_method_transform = method_list.transform.Find(current_class_name).Find(methodName);

            if (!existing_method_transform)
            {
                GameObject new_method = Instantiate(method_prefab);
                new_method.transform.SetParent(method_list.transform.Find(current_class_name), false);
                new_method.GetComponent<TextMeshProUGUI>().text = methodName;
                new_method.name = methodName;

                //Assign method variables
                BrowserMethod new_method_component = new_method.GetComponent<BrowserMethod>();
                new_method_component.sourceCode = input_code;
                new_method_component.field = this.gameObject.GetComponent<TMP_InputField>();
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
    }
}