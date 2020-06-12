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
using PharoModule;
using System.Globalization;
using System.Linq;
using System.Runtime.Serialization.Formatters.Binary;
using Unity.VectorGraphics;
using System.Security.Cryptography;
using System.Threading;
using LoggingModule;
using ImageUtils;

public class PlaygroundTextEditor : TextEditorBehaviour
{
    public InspectorInit inspector_prefab;
    public SVGObjectInit svg_prefab;
    private float width;
    private float height;

    void Start()
    {
        Vector2 sd = GetComponent<RectTransform>().sizeDelta;
        width = sd.x;
        height = sd.y;
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            if (!(Input.GetKey(KeyCode.LeftCommand) ||
                    Input.GetKey(KeyCode.LeftControl)))
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
                else { }
            }
            else { }
        }
    }

    async void PharoPrint()
    {
        string clean_code = cleanCode(field.text);
        string responseString = await Pharo.Print(clean_code);
        string output = "";
        try
        {
            if(Regex.Match(clean_code, @"visualize(2D)?(\s*)\.").Success)
            {
                Sprite sprite = null;
                if (Regex.Match(clean_code, @"visualize(\s*)\.").Success)
                {
                    responseString = Regex.Replace(responseString, @"#|\[|\]|\n|( 0)*", "");
                    byte[] byteArray = responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
                    string file_path = Application.persistentDataPath + @"\temp";
                    File.WriteAllBytes(file_path, byteArray);
                    sprite = ImageModule.ImportSVG(file_path);
                }
                else if (Regex.Match(clean_code, @"visualize2D(\s*)\.").Success)
                {
                    responseString = Regex.Replace(responseString, @"#|\[|\]|\n", "");
                    byte[] byteArray = responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
                    Texture2D tex = new Texture2D(2, 2);
                    tex.LoadImage(byteArray);
                    sprite = Sprite.Create(
                        tex,
                        new Rect(0, 0, tex.width, tex.height),
                        Vector2.zero
                    );
                }
                SVGObjectInit instance = Instantiate(svg_prefab);
                instance.setSprite(sprite);
                instance.Initialize(
                    transform.position,
                    transform.TransformPoint(new Vector3(-0.75f * width, -0.5f * height, 0)),
                    transform.forward,
                    player
                );
                responseString = "a RTBuilder\n";
            }
            output = " <color=#b32d00>" + responseString.Remove(responseString.LastIndexOf("\n"), 1) + "</color>";
            InteractionLogger.RegisterCodeExecution(clean_code, responseString);
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
            start = Interlocked.Exchange(ref end, start);
        int selection_length = end - start;
        
        // Assuming selection is the variable to inspect, we send the message
        string selection = clean_code.Substring(start, selection_length);
        string res = await Pharo.Inspect(clean_code, selection);

        if (!Regex.Match(res, @"\[Error\](.*)").Success)
        {
            Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.6f * width, 0, 0));
            InspectorInit new_inspector = Instantiate(inspector_prefab);
            new_inspector.setContent(res);
            new_inspector.Initialize(
                new Vector3(transform.position.x, 2, transform.position.z),
                new Vector3(newWorldPos.x, 2, newWorldPos.z),
                transform.forward,
                player
            );
        }
        else
            field.text += " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";

        InteractionLogger.RegisterCodeInspection(selection, res);
    }

    public override void onSelect()
    {
        base.onSelect();
        InteractionLogger.StartTimerFor("Playground");
    }

    public override void onDeselect()
    {
        base.onDeselect();
        InteractionLogger.EndTimerFor("Playground");
    }
}
