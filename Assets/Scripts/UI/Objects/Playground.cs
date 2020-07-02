using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Threading;
using System.Net.Http;
using UnityEngine.UI;
using SaveAndLoad;
using PharoModule;
using LoggingModule;
using TMPro;
using InstantiatorModule;
using System.Security.Cryptography;

public class Playground : InitializeBehaviour
{
    private Graph view;
    private Inspector insp;

    async void PharoDo()
    {
        string output = "";
        try
        {
            string selectedCode = getSelectedCode(cleanCode(field.text));

            // Getting Transcripts
            foreach (Match m in Regex.Matches(selectedCode, @"VRIDE\s*log:.*\."))
            {
                string response = await Pharo.Print(m.Value);
                response = response.Replace("'", "").Replace("\"", "");
                VRIDEController.transcriptContents += response + "\n";
            }

            // Execution
            string responseString = await Pharo.Print(selectedCode);
            if (Regex.Match(selectedCode, @"visualize(\s*)(asSVG|asPNG)(\s*)\.").Success)
            {
                string type = Regex.Match(selectedCode, @"visualize(\s*)asSVG(\s*)\.").Success ? "SVG" : "PNG";
                if (view == null)
                {
                    float width = GetComponent<RectTransform>().sizeDelta.x;
                    float height = GetComponent<RectTransform>().sizeDelta.y;
                    view = Instantiator.Graph() as Graph;
                    player.graphs.Add(view);
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
                if(insp == null)
                {
                    float width = GetComponent<RectTransform>().sizeDelta.x;
                    Vector3 newWorldPos = transform.TransformPoint(new Vector3(width, 0, 0));
                    insp = Instantiator.Inspector() as Inspector;
                    player.inspectors.Add(insp);
                    insp.Initialize(
                        new Vector3(transform.position.x, 2, transform.position.z),
                        new Vector3(newWorldPos.x, 2, newWorldPos.z),
                        transform.forward,
                        player
                    );
                    InteractionLogger.Count("Inspector");
                }
                insp.setContent(res);
            }
            else
                output = " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            InteractionLogger.RegisterCodeInspection(selection, res);
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

    async void PharoBrowse()
    {
        string selection = getSelectedCode(cleanCode(field.text));
        string packageName = "";
        string className = "";
        foreach (KeyValuePair<string, List<Tuple<string, string>>> keyVal in VRIDEController.data.classes)
        {
            packageName = keyVal.Key;
            foreach (Tuple<string, string> t in keyVal.Value)
            {
                if (t.Item1 == selection)
                {
                    className = t.Item1;
                    Browser b = Instantiator.Browser(VRIDEController.data) as Browser;
                    b.package_list.transform.Find(packageName).gameObject.GetComponent<BrowserPackage>().click();
                    b.class_list.Find(packageName).Find(className).gameObject.GetComponent<BrowserClass>().click();
                    b.Initialize(
                        transform.position,
                        transform.position,
                        transform.forward,
                        player
                    );
                    break;
                }
            }
            if(className != "")
                break;
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

    public override void innerBehaviour() {
        if ((Input.anyKeyDown || Input.GetKeyUp(KeyCode.Backspace)) && field.isFocused)
        {
            bool leftCmd = Input.GetKey(KeyCode.LeftCommand);
            bool leftCtrl = Input.GetKey(KeyCode.LeftControl);
            bool f3 = Input.GetKeyDown(KeyCode.F3);
            bool f4 = Input.GetKeyDown(KeyCode.F4);
            bool f5 = Input.GetKeyDown(KeyCode.F5);
            bool f8 = Input.GetKeyDown(KeyCode.F8);
            bool p = Input.GetKeyDown("p");
            bool d = Input.GetKeyDown("d");
            bool i = Input.GetKeyDown("i");
            bool c = Input.GetKeyDown("c");
            bool v = Input.GetKeyDown("v");
            bool b = Input.GetKeyDown("b");

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
                else if (((leftCmd || leftCtrl) && b) || f8)
                    PharoBrowse();
                else if (((leftCmd || leftCtrl) && v) || ((leftCmd || leftCtrl) && c))
                    onChangeInput();
            }
        }
    }

    public override void onClose()
    {
        player.playgrounds.Remove(this);
        InteractionLogger.Discount("Playground");
        Destroy(gameObject);
    }
}
