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
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f3 = Input.GetKey(KeyCode.F3);
            bool f4 = Input.GetKey(KeyCode.F4);
            bool f5 = Input.GetKey(KeyCode.F5);
            bool p = Input.GetKey("p");
            bool d = Input.GetKey("d");
            bool i = Input.GetKey("i");
            bool c = Input.GetKey("c");
            bool v = Input.GetKey("v");

            if (!(leftCmd || leftCtrl || f3 || f4 || f5))
                onChangeInput();
            else
            {
                if (((leftCmd || leftCtrl) && d) || f3)
                    PharoDo();
                else if (((leftCmd || leftCtrl) && p) || f4)
                    PharoPrint();
                else if (((leftCmd || leftCtrl) && i) || f5)
                    PharoInspect();
                else if ((leftCmd || leftCtrl) && v)
                    onChangeInput();
                else if ((leftCmd || leftCtrl) && c)
                    onChangeInput();
                else { }
            }
        }
    }

    async void PharoDo()
    {
        string clean_code = cleanCode(field.text);
        string selectedCode = getSelectedCode(clean_code);
        string responseString = await Pharo.Print(selectedCode);
        string output = "";
        try
        {
            if (Regex.Match(selectedCode, @"visualize(\s*)(asSVG|asPNG)(\s*)\.").Success)
            {
                Sprite sprite = null;
                if (Regex.Match(selectedCode, @"visualize(\s*)asSVG(\s*)\.").Success)
                {
                    responseString = Regex.Replace(responseString, @"#|\[|\]|\n|( 0)*", "");
                    byte[] byteArray = responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
                    string file_path = Application.persistentDataPath + @"\temp";
                    File.WriteAllBytes(file_path, byteArray);
                    sprite = ImageModule.ImportSVG(file_path);
                }
                else
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
            }
            PharoInspect();
            InteractionLogger.RegisterCodeExecution(selectedCode, responseString);
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

    async void PharoPrint()
    {
        string clean_code = cleanCode(field.text);
        string selection = getSelectedCode(clean_code);
        string res = await Pharo.Print(selection);
        string output = "";
        try
        {
            output = " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            InteractionLogger.RegisterCodeExecution(selection, res);
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
        string selection = getSelectedCode(clean_code);
        string res = await Pharo.Inspect(selection);
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
