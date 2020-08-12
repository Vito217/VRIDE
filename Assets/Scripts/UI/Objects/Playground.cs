using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using PharoModule;
using LoggingModule;

public class Playground : InitializeBehaviour
{
    private Graph view;
    private Inspector insp;

    async void PharoDo()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            string selectedCode = getSelectedCode(cleanCode(field.text));

            // Getting Transcripts
            foreach (Match m in Regex.Matches(selectedCode,
                @"VRIDE[\n\s]+log:[\n\s\t]+(.*)(\.|\s*\Z)"))
            { 
                string response = await Pharo.Print(m.Groups[1].Value);
                response = response.Replace("'", "").Replace("\"", "");
                VRIDEController.transcriptContents += response + "\n";
            }
            selectedCode = Regex.Replace(selectedCode, 
                @"VRIDE[\n\s]+log:[\n\s\t]+(.*)(\.|\s*\Z)", "");

            Debug.Log(selectedCode);

            string pattern = @"(\A|[\n\s]+)([a-zA-Z0-9]+)[\n\s]+visualize[\n\s]+(asSVG|asPNG)[\n\s]+?\.";
            MatchCollection matches = Regex.Matches(selectedCode, pattern);
            if (matches.Count > 0)
            {
                string var = matches[matches.Count - 1].Groups[2].Value;
                string type = matches[matches.Count - 1].Groups[3].Value.Substring(2);

                string exporter = (type == "PNG") ?
                    " RTPNGExporter new builder: (" + var + " view); fileName: 'img.png'; exportToFile. " :
                    " RTSVGExporter new view: (" + var + " view); fileName: 'img.svg'; exportToFile. ";

                string finalCode = 
                    exporter +
                    "(FileLocator workingDirectory / 'img." + type.ToLower() + "') " + 
                        "binaryReadStreamDo:[ :stream | stream upToEnd ].";

                selectedCode = Regex.Replace(selectedCode, pattern, finalCode);

                // Execution
                string responseString = await Pharo.Print(selectedCode);
                if (!responseString.Contains("[Error]"))
                {
                    if (view == null)
                    {
                        float width = GetComponent<RectTransform>().sizeDelta.x;
                        float height = GetComponent<RectTransform>().sizeDelta.y;
                        view = Instantiator.Instance.Graph() as Graph;
                        player.graphs.Add(view);
                        view.Initialize(
                            transform.position,
                            transform.TransformPoint(new Vector3(-width, 0, 0)),
                            transform.forward,
                            player
                        );
                        InteractionLogger.Count("GraphObject");
                    }
                    view.setSprite(responseString, type);
                }
                else
                {
                    output = " -> " + responseString.Remove(responseString.LastIndexOf("\n"), 1);
                }
                InteractionLogger.RegisterCodeExecution(selectedCode, responseString);
            }
            else if(!(Regex.Match(selectedCode, @"(\A[\n\t\s]+\Z)").Success || 
                selectedCode == ""))
            {
                await PharoInspect();
            }
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            output = " -> [Error] " + e.Message;
        }
        finally
        {
            field.text += output;
        }
        Reactivate();
    }

    async void PharoPrint()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text));

            foreach (Match m in Regex.Matches(selection,
                @"VRIDE[\n\s]+log:[\n\s\t]+(.*)(\.|\s*\Z)"))
            {
                string response = await Pharo.Print(m.Groups[1].Value);
                response = response.Replace("'", "").Replace("\"", "");
                VRIDEController.transcriptContents += response + "\n";
            }
            selection = Regex.Replace(selection,
                @"VRIDE[\n\s]+log:[\n\s\t]+(.*)(\.|\s*\Z)", "");

            string res = await Pharo.Print(selection);
            //output = " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            output = " -> " + res.Remove(res.LastIndexOf("\n"), 1);
            InteractionLogger.RegisterCodeExecution(selection, res);
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            output = " -> [Error] " + e.Message;
        }
        finally
        {
            field.text += output;
        }
        Reactivate();
    }

    async Task PharoInspect()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text));
            string res = await Pharo.Inspect(selection);
            if (res.Contains("OrderedCollection"))
            {
                if(insp == null)
                {
                    float width = GetComponent<RectTransform>().sizeDelta.x;
                    Vector3 newWorldPos = transform.TransformPoint(new Vector3(width, 0, 0));
                    insp = Instantiator.Instance.Inspector() as Inspector;
                    player.inspectors.Add(insp);
                    insp.Initialize(
                        transform.position,
                        newWorldPos,
                        transform.forward,
                        player
                    );
                    InteractionLogger.Count("Inspector");
                }
                insp.setContent(res);
            }
            else
            {
                //output = " <color=#b32d00>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
                output = " -> " + res.Remove(res.LastIndexOf("\n"), 1);
            }

            InteractionLogger.RegisterCodeInspection(selection, res);
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            output = " -> [Error] " + e.Message;
        }
        finally
        {
            field.text += output;
        }
        Reactivate();
    }

    void PharoBrowse()
    {
        string selection = getSelectedCode(cleanCode(field.text));
        string packageName;
        string className = "";
        foreach (KeyValuePair<string,
                 SortedDictionary<string, (string classCode,
                    List<(string methodName, string methodCode, string side)> classMethods)>> 
                        keyVal in VRIDEController.sysData.data)
        {
            packageName = keyVal.Key;
            foreach (KeyValuePair<string, (string classCode,
                List<(string methodName, string methodCode, string side)> classMethods)> 
                    t in keyVal.Value)
            {
                if (t.Key == selection)
                {
                    className = t.Key;
                    Browser b = Instantiator.Instance.Browser();
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

    public override void onClose()
    {
        player.playgrounds.Remove(this);
        InteractionLogger.Discount("Playground");
        Destroy(gameObject);
    }

    public override void innerBehaviour()
    {
        if (Input.anyKeyDown && field.isFocused && !loadingWheel.active)
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
            else
                onChangeInput();
        }
    }
}
