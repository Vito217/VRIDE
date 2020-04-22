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
    public string IP = "http://localhost:1701/repl";

    /*void Start()
    {
        Regex rgx = new Regex(@"(\A(.*:?) )|(\A(.*:?)\n)");
        string line = "name: value\nname := value";
        string methodName = rgx.Matches(line)[0].Value;
        Debug.Log(methodName);
    }*/

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetMouseButton(0) || Input.GetMouseButton(1)))
            {
                onChangeInput();
            }
            else if (Input.GetKey(KeyCode.LeftControl) && Input.GetKey("g"))
            {
                PharoDefine(field.text);
            }
        }
    }

    async void PharoDefine(string input_code)
    {
        // Cleaning code from RichText
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

            // Getting class name
            Regex rgx = new Regex(@"\s#(.*)(\s|\n)");
            string className = rgx.Matches(clean_code)[0].Groups[1].Value;

            // Check if class object exists
            GameObject existing_class = GameObject.Find("/Browser/Classes/Panel/Scroll View/Viewport/Content/" + className);
            if (!existing_class)
            {
                // Create new browser class
                GameObject new_class = Instantiate(class_prefab);
                new_class.transform.SetParent(GameObject.Find("/Browser/Classes/Panel/Scroll View/Viewport/Content").transform, false);
                new_class.GetComponent<TextMeshProUGUI>().text = className;
                new_class.name = className;

                // Create method list
                GameObject new_method_list = Instantiate(content_list_prefab);
                new_method_list.transform.SetParent(GameObject.Find("/Browser/Methods/Panel/Scroll View/Viewport/Content").transform, false);
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
                new_method_component.sourceCode = "initialize super initialize .";
                new_method_component.field = this.gameObject.GetComponent<TMP_InputField>();
            }
            else
            {
                //Update class source code
                BrowserClass existing_component = existing_class.GetComponent<BrowserClass>();
                existing_component.sourceCode = input_code;
            }

        }
        else
        {
            GameObject class_window = GameObject.Find("/Browser/Classes/Panel/Scroll View/Viewport/Content");
            BrowserClass current_class = class_window.GetComponent<ClassWindow>().getLastSelectedClass();
            string current_class_name = current_class.name;

            // Executing pharo code
            clean_code = clean_code.Replace("'", "''");
            string method_code = current_class_name + " compile: '"+ clean_code +"'";
            var response = await client.PostAsync(IP, new StringContent(method_code, Encoding.UTF8));
            var responseString = await response.Content.ReadAsStringAsync();

            // Getting method name
            Regex rgx = new Regex(@"(\A(.*:?) )|(\A(.*:?)\n)");
            string methodName = rgx.Matches(clean_code)[0].Value;

            GameObject existing_method = 
                GameObject.Find("/Browser/Methods/Panel/Scroll View/Viewport/Content/" + current_class_name + "/" + methodName);

            if (!existing_method)
            {
                GameObject new_method = Instantiate(method_prefab);
                new_method.transform.SetParent(GameObject.Find("/Browser/Methods/Panel/Scroll View/Viewport/Content/" + current_class_name).transform, false);
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
                BrowserMethod existing_component = existing_method.GetComponent<BrowserMethod>();
                existing_component.sourceCode = input_code;
            }
        }
    }
}
