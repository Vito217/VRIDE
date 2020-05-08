using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;
using System.Linq.Expressions;
using System.Security.Cryptography;

public class PlaygroundTextEditor : TextEditorBehaviour
{
    public GameObject inspector_prefab;
    private int out_index = -1;

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if(out_index > 0 && field.caretPosition <= out_index)
            {
                field.text = Regex.Replace(field.text, @" <color=#b32d00>((.*)\n)*</color>", "");
                out_index = -1;
            }

            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetMouseButton(0) || Input.GetMouseButton(1)))
                onChangeInput();
            else if (Input.GetKey(KeyCode.LeftControl))
            {
                if (Input.GetKeyDown("g"))
                    PharoPrint();
                else if (Input.GetKeyDown("h"))
                    PharoInspect();
                else if (Input.GetKeyDown("v"))
                    onChangeInput();
                else if (Input.GetKeyDown("c"))
                {
                    // Do Nothing
                }
            }
        }
    }

    async void PharoPrint()
    {
        string clean_code = field.text;
        clean_code = Regex.Replace(clean_code, @" <color=#b32d00>((.*)\n)*</color>", "");
        clean_code = clean_code.Replace("<color=#00ffffff>", "");
        clean_code = clean_code.Replace("</color>", "");
        clean_code = clean_code.Replace("<b>", "");
        clean_code = clean_code.Replace("</b>", "");
        clean_code = clean_code.Replace("<br>", " ");

        var content = new StringContent(clean_code, Encoding.UTF8);
        var response = await client.PostAsync("http://localhost:1701/repl", content);
        var responseString = await response.Content.ReadAsStringAsync();
        string output = " <color=#b32d00>" + responseString + "</color>";
        out_index = clean_code.Length;
        field.text += output;
    }


    async void PharoInspect()
    {
        string clean_code = field.text;
        clean_code = Regex.Replace(clean_code, @" <color=#b32d00>((.*)\n)*</color>", "");
        clean_code = clean_code.Replace("<color=#00ffffff>", "");
        clean_code = clean_code.Replace("</color>", "");
        clean_code = clean_code.Replace("<b>", "");
        clean_code = clean_code.Replace("</b>", "");
        clean_code = clean_code.Replace("<br>", " ");

        // Getting selected text
        int start = field.selectionAnchorPosition;
        int end = field.caretPosition;
        if (end < start)
        {
            int aux = end;
            end = start;
            start = aux;
        }
        string text = clean_code;
        int selection_length = end - start;
        string selection = text.Substring(start, selection_length);

        // Assuming selection is the variable to inspect,
        // we send the message
        string code = clean_code + "\n" +
            "tuples := OrderedCollection new.\n" +
            "tuples addLast: 'self=',(" + selection + " value asString).\n" +
            selection + " class instVarNames do: [ :each |\n" +
                "\ttuples addLast: (each asString),'=', ((" + selection + " instVarNamed: each value) asString).\n" +
            "].\n" +
            "tuples .";

        var content = new StringContent(code, Encoding.UTF8);
        var response = await client.PostAsync(IP, content);
        var responseString = await response.Content.ReadAsStringAsync();

        GameObject new_inspector = Instantiate(inspector_prefab);
        new_inspector.transform.forward = this.transform.forward;
        new_inspector.transform.position = this.transform.position;

        if (inspector_prefab.name.StartsWith("OVR"))
        {
            Camera camera = transform.parent.parent.gameObject.GetComponent<Camera>();
            GameObject laser_pointer = GameObject.Find("/OVREventSystem/LaserPointer");

            new_inspector.transform.Find("InspectorTable").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;
            new_inspector.transform.Find("TextEditor").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;

            new_inspector.transform.Find("InspectorTable").gameObject.GetComponent<Canvas>().worldCamera = camera;
            new_inspector.transform.Find("TextEditor").gameObject.GetComponent<Canvas>().worldCamera = camera;
        }

        float width = this.gameObject.GetComponent<RectTransform>().sizeDelta.x;
        float height = this.gameObject.GetComponent<RectTransform>().sizeDelta.y;
        Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.75f*width, -0.5f*height, 0));
        new_inspector.GetComponent<InspectorInit>().new_pos = newWorldPos;
        new_inspector.GetComponent<InspectorInit>().initializeContent(responseString);
        new_inspector.GetComponent<InspectorInit>().initializing = true;
    }
}
