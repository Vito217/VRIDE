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

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl) || Input.GetKey(KeyCode.LeftCommand) || Input.GetMouseButton(0) || Input.GetMouseButton(1))) 
                onChangeInput();
            else if (Input.GetKey(KeyCode.LeftControl) || Input.GetKey(KeyCode.LeftCommand))
            {
                if (Input.GetKeyDown("g"))
                    PharoPrint();
                else if (Input.GetKeyDown("h"))
                    PharoInspect();
                else if (Input.GetKeyDown("v"))
                    onChangeInput();
                else if (Input.GetKeyDown("c"))
                    onChangeInput();
            }
        }
    }

    async void PharoPrint()
    {
        string clean_code = cleanCode(field.text);
        string final_code =
            "[" + clean_code + "]\n" +
                "\ton: Error\n" +
                "\tdo: [:e | '[Error] ' , (e message lookupClass name), ': ' , (e messageText)].";

        var content = new StringContent(final_code, Encoding.UTF8);
        var response = await client.PostAsync(IP, content);
        string responseString = await response.Content.ReadAsStringAsync();
        string output = "";

        try
        {
            if (Regex.Match(clean_code, @"visualize(\s*)\.").Success)
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
                Sprite sprite = VectorUtils.BuildSprite(geoms, 100.0f, VectorUtils.Alignment.Center, Vector2.zero, 128, true);
                
                GameObject instance = Instantiate(svg_prefab) as GameObject;
                instance.GetComponent<SpriteRenderer>().sprite = sprite;
                instance.transform.position = new Vector3(transform.position.x, instance.GetComponent<SpriteRenderer>().bounds.size.y * 0.5f, transform.position.z);
                
                GameObject toolbar = instance.transform.Find("Toolbar").gameObject;
                toolbar.GetComponent<ToolbarBehaviour>().player = player;
                toolbar.transform.position = new Vector3(toolbar.transform.position.x, instance.GetComponent<SpriteRenderer>().bounds.size.y + 0.125f, toolbar.transform.position.z);

                float width = this.gameObject.GetComponent<RectTransform>().sizeDelta.x;
                float height = this.gameObject.GetComponent<RectTransform>().sizeDelta.y;
                Vector3 newWorldPos = transform.TransformPoint(new Vector3(-0.75f * width, -0.5f * height, 0));
                instance.GetComponent<SVGObjectInit>().new_pos = newWorldPos;
                instance.GetComponent<SVGObjectInit>().initializing = true;
            }
            else if (Regex.Match(clean_code, @"visualize2D(\s*)\.").Success)
            {
                responseString = Regex.Replace(responseString, @"#|\[|\]|\n", "");
                byte[] byteArray = responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
                string file_path = Application.persistentDataPath + @"\temp";
                File.WriteAllBytes(file_path, byteArray);

                Texture2D tex = new Texture2D(2, 2);
                tex.LoadImage(byteArray);
                Sprite sprite = Sprite.Create(tex, new Rect(0, 0, tex.width, tex.height), new Vector2(tex.width * 0.5f, tex.height * 0.5f));
                GameObject instance = Instantiate(svg_prefab) as GameObject;
                instance.GetComponent<SpriteRenderer>().sprite = sprite;
            }
            else
            {
                output = " <color=#b32d00>" + responseString + "</color>";
                output = output.Remove(output.LastIndexOf("\n"), 1);
            }
        }
        catch (Exception e)
        {
            output = " <color=#b32d00>[Error] " + e.Message + "</color>";
        }
        finally
        {
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

        string code = clean_code + ".\n" +
            "tuples := OrderedCollection new.\n" +
            "tuples addLast: 'self=',(" + selection + " value asString).\n" +
            selection + " class instVarNames do: [ :each |\n" +
                "\ttuples addLast: (each asString),'=', ((" + selection + " instVarNamed: each value) asString).\n" +
            "].\n" +
            "tuples .";

        string final_code =
            "[" + code + "]\n" +
                "\ton: Error\n" +
                "\tdo: [:e | '[Error] ' , (e message lookupClass name), ': ' , (e messageText)].";

        var content = new StringContent(code, Encoding.UTF8);
        var response = await client.PostAsync(IP, content);
        string responseString = await response.Content.ReadAsStringAsync();

        if (!Regex.Match(responseString, @"\[Error\](.*)").Success)
        {
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
            Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.75f * width, -0.5f * height, 0));
            new_inspector.GetComponent<InspectorInit>().new_pos = newWorldPos;
            new_inspector.GetComponent<InspectorInit>().initializeContent(responseString);
            new_inspector.GetComponent<InspectorInit>().initializing = true;
        }
        else
        {
            string output = " <color=#b32d00>" + responseString + "</color>";
            output = output.Remove(output.LastIndexOf("\n"), 1);
            field.text += output;
        }
    }
}
