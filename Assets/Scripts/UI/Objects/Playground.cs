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

public class Playground : InitializeBehaviour
{
    private Graph view;

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
                float width = GetComponent<RectTransform>().sizeDelta.x;
                Vector3 newWorldPos = transform.TransformPoint(new Vector3(1.6f * width, 0, 0));
                Inspector new_inspector = Instantiator.Inspector() as Inspector;
                player.inspectors.Add(new_inspector);
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
        catch (Exception e)
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

    public override void innerBehaviour() {
        if ((Input.anyKeyDown || Input.GetKeyUp(KeyCode.Backspace)) && field.isFocused)
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

    public override void onClose()
    {
        player.playgrounds.Remove(this);
        InteractionLogger.Discount("Playground");
        Destroy(gameObject);
    }
}
