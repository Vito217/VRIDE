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
using InstantiatorModule;
using System.Globalization;
using System.Runtime.Serialization.Formatters.Binary;
using Unity.VectorGraphics;
using System.Security.Cryptography;
using LoggingModule;
using ImageUtils;
using System.Media;

public class PlaygroundTextEditor : TextEditorBehaviour
{
    private SVGObjectInit view;
    private float width;
    private float height;

    void Start()
    {
        Vector2 sd = GetComponent<RectTransform>().sizeDelta;
        width = sd.x;
        height = sd.y;
    }

    void Update()
    {
        if (Input.anyKeyDown && field.isFocused)
        {
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f3 = Input.GetKeyDown(KeyCode.F3);
            bool f4 = Input.GetKeyDown(KeyCode.F4);
            bool f5 = Input.GetKeyDown(KeyCode.F5);
            bool p = Input.GetKeyDown("p");
            bool d = Input.GetKeyDown("d");
            bool i = Input.GetKeyDown("i");
            bool c = Input.GetKeyDown("c");
            bool v = Input.GetKeyDown("v");

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
                else if (((leftCmd || leftCtrl) && v) || ((leftCmd || leftCtrl) && c))
                    onChangeInput();
            }
        }
    }

    async void PharoDo()
    {
        string output = "";
        try
        {
            /**
            // Getting Transcripts
            foreach (Match m in Regex.Matches(selectedCode, @"Transcript\sshow:.*\."))
            {
                string response = await Pharo.Transcript(m.Value);
                response = Regex.Match(response, @"'stepContents=.*'\)").Value;
                Debug.Log(response);
                response = response.Replace("stepContents=", "").Replace("'", "").Replace(")", "");
                VRIDEController.transcriptContents += response + "\n";
            }
            foreach (GameObject t in player.GetComponent<VRIDEController>().transcripts)
            {
                t.transform.Find("Editor/Panel/InputField (TMP)").gameObject
                    .GetComponent<TMP_InputField>().text = VRIDEController.transcriptContents;
            }
            **/

            // Execution
            string selectedCode = getSelectedCode(cleanCode(field.text));
            string responseString = await Pharo.Print(selectedCode);
            if (Regex.Match(selectedCode, @"visualize(\s*)(asSVG|asPNG)(\s*)\.").Success)
            {
                string type = Regex.Match(selectedCode, @"visualize(\s*)asSVG(\s*)\.").Success ? "SVG" : "PNG";
                if (view == null)
                {
                    view = Instantiator.Graph() as SVGObjectInit;
                    player.GetComponent<VRIDEController>().graphs.Add(view.gameObject);
                    view.Initialize(
                        transform.position,
                        transform.TransformPoint(new Vector3(-0.75f * width, -0.5f * height, 0)),
                        transform.forward,
                        player
                    );
                    InteractionLogger.Count("GraphObject");
                }
                view.setSprite(responseString, type);
            }
            else
            {
                PharoInspect();
            }
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
        string output = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text));
            string res = await Pharo.Print(selection);
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
        string output = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text));
            string res = await Pharo.Inspect(selection);
            if (!Regex.Match(res, @"\[Error\](.*)").Success)
            {
                Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.6f * width, 0, 0));
                InspectorInit new_inspector = Instantiator.Inspector() as InspectorInit;
                player.GetComponent<VRIDEController>().inspectors.Add(new_inspector.gameObject);
                new_inspector.setContent(res);
                new_inspector.Initialize(
                    new Vector3(transform.position.x, 2, transform.position.z),
                    new Vector3(newWorldPos.x, 2, newWorldPos.z),
                    transform.forward,
                    player
                );
                InteractionLogger.Count("Inspector");
            }
            else
                output = " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            InteractionLogger.RegisterCodeInspection(selection, res);
        }
        catch(Exception e)
        {
            output = " <color=#b32d00>[Error] " + e.Message + "</color>";
        }
        finally
        {
            field.text += output;
        }
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
