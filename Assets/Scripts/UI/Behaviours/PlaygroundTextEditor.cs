using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEditor;
using UnityEngine.AddressableAssets;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;
using System.Globalization;
using System.Linq;
using System.Runtime.Serialization.Formatters.Binary;
using Unity.VectorGraphics;

public class PlaygroundTextEditor : TextEditorBehaviour
{
    public GameObject inspector_prefab;
    public GameObject svg_prefab;
    private int out_index = -1;

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetMouseButton(0) || Input.GetMouseButton(1))) {
                if ((out_index > 0 && field.caretPosition <= out_index) ||
                    (Input.GetKey(KeyCode.Return) && field.caretPosition >= out_index && out_index >= 0) ||
                    (Input.GetKey(KeyCode.Space) && field.caretPosition >= out_index && out_index >= 0))
                {
                    field.text = Regex.Replace(field.text, @"\s<color=#b32d00>.*", "");
                    field.caretPosition = out_index;
                    out_index = -1;
                }
                onChangeInput();
            }
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
                    field.text = Regex.Replace(field.text, @"\s<color=#b32d00>.*", "");
                    field.caretPosition = out_index;
                    out_index = -1;
                }
            }
        }
    }

    async void PharoPrint()
    {
        string clean_code = cleanCode(field.text);

        //Debug.Log(clean_code);

        string final_code =
            "[" + clean_code + "]\n" +
                "\ton: Error\n" +
                "\tdo: [:e | '[Error] ' , (e message lookupClass name), ': ' , (e messageText)].";

        var content = new StringContent(final_code, Encoding.UTF8);
        var response = await client.PostAsync(IP, content);
        string responseString = await response.Content.ReadAsStringAsync();

        if (Regex.Match(clean_code, @"visualize(2D)?(\s*)\.").Success)
        {

            responseString = Regex.Replace(responseString, @"#|\[|\]|\n|( 0)*", "");
            byte[] byteArray = responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
            string file_path = Application.persistentDataPath + @"\temp";
            File.WriteAllBytes(file_path, byteArray);

            var tessOptions = new VectorUtils.TessellationOptions()
            {
                StepDistance = 100.0f,
                MaxCordDeviation = 0.5f,
                MaxTanAngleDeviation = 0.1f,
                SamplingStepSize = 0.01f
            };

            var sceneInfo = SVGParser.ImportSVG(new StreamReader(file_path));
            var geoms = VectorUtils.TessellateScene(sceneInfo.Scene, tessOptions);
            var sprite = VectorUtils.BuildSprite(geoms, 10.0f, VectorUtils.Alignment.Center, Vector2.zero, 128, true);
            GameObject instance = Instantiate(svg_prefab) as GameObject;
            instance.GetComponent<SpriteRenderer>().sprite = sprite;
            //File.Delete(file_path);
        }
        else
        {
            string output = " <color=#b32d00>" + responseString + "</color>";
            output = output.Remove(output.LastIndexOf("\n"), 1);
            out_index = clean_code.Length;
            field.text += output;
        }
    }


    async void PharoInspect()
    {
        string clean_code = cleanCode(field.text);

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

        //if (inspector_prefab.name.StartsWith("OVR"))
        //{
        //    Camera camera = transform.parent.parent.gameObject.GetComponent<Camera>();
        //    GameObject laser_pointer = GameObject.Find("/OVREventSystem/LaserPointer");

        //    new_inspector.transform.Find("InspectorTable").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;
        //    new_inspector.transform.Find("TextEditor").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;

        //    new_inspector.transform.Find("InspectorTable").gameObject.GetComponent<Canvas>().worldCamera = camera;
        //    new_inspector.transform.Find("TextEditor").gameObject.GetComponent<Canvas>().worldCamera = camera;
        //}

        float width = this.gameObject.GetComponent<RectTransform>().sizeDelta.x;
        float height = this.gameObject.GetComponent<RectTransform>().sizeDelta.y;
        Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.75f*width, -0.5f*height, 0));
        new_inspector.GetComponent<InspectorInit>().new_pos = newWorldPos;
        new_inspector.GetComponent<InspectorInit>().initializeContent(responseString);
        new_inspector.GetComponent<InspectorInit>().initializing = true;
    }
}
