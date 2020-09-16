using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using PharoModule;
using LoggingModule;
using SaveAndLoad;

public class Playground : InitializeBehaviour
{
    private Graph view;
    private Inspector insp;

    public async void PharoDo()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            string selectedCode = getSelectedCode(cleanCode(field.text));

            // Getting Transcripts
            string transcriptPattern =
                @"(VRIDE[\n\s\t]+log:[\n\s\t]+|Transcript[\n\s\t]+show:[\n\s\t]+)(.*)(\.|\Z)";
            foreach (Match m in Regex.Matches(selectedCode, transcriptPattern))
            {
                string response = await Pharo.Print(m.Groups[2].Value);
                response = response.Replace("'", "").Replace("\"", "");
                SaveAndLoadModule.transcriptContents += response + "\n";
            }
            selectedCode = Regex.Replace(selectedCode, transcriptPattern, "");

            // Roassal
            /*
            string pattern =
                @"(\A|\.)[\n\t\s]*([a-zA-Z0-9]+)[\n\s\t]+visualize[\n\s\t]+as(SVG|PNG)[\n\s\t]*\.";
            MatchCollection matches = Regex.Matches(selectedCode, pattern);
            if (matches.Count > 0)
            {
                string var = matches[matches.Count - 1].Groups[2].Value;
                string type = matches[matches.Count - 1].Groups[3].Value;
                string lowerType = type.ToLower();
                string exporter;

                if (Regex.Match(selectedCode, @"RS.*[\s\t\n]+new").Success)
                    exporter =
                        ". " + var + " canvas " + lowerType + "Exporter " +
                            "noFixedShapes; " +
                            "fileName: 'img." + lowerType + "'; " +
                            "export. ";
                else
                    exporter =
                        ". RT" + type + "Exporter new " +
                            (type == "PNG" ? "builder" : "view") + ": (" + var + " view); " +
                            "fileName: 'img." + lowerType + "'; " +
                            "exportToFile. ";

                string finalCode =
                        exporter +
                        "(FileLocator workingDirectory / 'img." + type.ToLower() + "') " +
                            "binaryReadStreamDo:[ :stream | stream upToEnd ].";

                selectedCode = Regex.Replace(selectedCode, pattern, finalCode);
                string responseString = await Pharo.Print(selectedCode);
                if (Regex.Match(responseString, @"\[[0-9\s]+\]").Success)
                {
                    if (view == null)
                    {
                        float width = GetComponent<RectTransform>().sizeDelta.x;
                        float height = GetComponent<RectTransform>().sizeDelta.y;
                        view = Instantiator.Instance.Graph() as Graph;
                        SaveAndLoadModule.graphs.Add(view);
                        view.Initialize(
                            transform.TransformPoint(new Vector3(-width, 0, 0)),
                            transform.forward
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
            */
            MatchCollection matches = Regex.Matches(selectedCode,
                @"([a-zA-Z0-9]+)([\n\s\t]*)(:=)([\n\s\t]*)((?!Example)((RS|RT).*))([\n\s\t]+)(new)([\n\s\t]*)(\.)");
            if (matches.Count > 0)
            {
                string type = "PNG";
                string responseString = await TryGetImageFile(matches, "PNG", selectedCode);
                if(!Regex.Match(responseString, @"\[[0-9\s]+\]").Success)
                {
                    type = "SVG";
                    responseString = await TryGetImageFile(matches, "SVG", selectedCode);
                }

                if (Regex.Match(responseString, @"\[[0-9\s]+\]").Success)
                {
                    if (view == null)
                    {
                        view = Instantiator.Instance.Graph() as Graph;
                        SaveAndLoadModule.graphs.Add(view);
                        InteractionLogger.Count("GraphObject");
                    }
                    view.setSprite(responseString, type);
                    float width = GetComponent<RectTransform>().sizeDelta.x;
                    view.Initialize(
                        transform.TransformPoint(new Vector3(
                            -0.5f * (width + view.GetComponent<RectTransform>().sizeDelta.x), 0, 0)),
                        transform.forward
                    );
                }
                else
                {
                    output = " -> " + responseString.Remove(responseString.LastIndexOf("\n"), 1);
                }
                InteractionLogger.RegisterCodeExecution(selectedCode, responseString);
            }
            else if (!String.IsNullOrWhiteSpace(selectedCode))
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

    public async void PharoPrint()
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
                SaveAndLoadModule.transcriptContents += response + "\n";
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

    public async Task PharoInspect()
    {
        DeactivateTemporarily();
        string output = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text));
            string res = await Pharo.Inspect(selection);
            if (res.Contains("OrderedCollection"))
            {
                if (insp == null)
                {
                    float width = GetComponent<RectTransform>().sizeDelta.x;
                    Vector3 newWorldPos = transform.TransformPoint(new Vector3(width, 0, 0));
                    insp = Instantiator.Instance.Inspector() as Inspector;
                    SaveAndLoadModule.inspectors.Add(insp);
                    insp.Initialize(
                        newWorldPos,
                        transform.forward
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

    /**void PharoBrowse()
    {
        string selection = getSelectedCode(cleanCode(field.text));
        string packageName;
        string className = "";
        foreach (KeyValuePair<string,
                 SortedDictionary<string, (string classCode,
                    List<(string methodName, string methodCode, string side)> classMethods)>> 
                        keyVal in SaveAndLoadModule.sysData.data)
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
                    b.class_list.transform.Find(className).gameObject.GetComponent<BrowserClass>().click();
                    b.Initialize(
                        transform.position,
                        transform.position,
                        transform.forward
                    );
                    break;
                }
            }
            if(className != "")
                break;
        }
    }**/

    public override void OnSelect(BaseEventData data)
    {
        base.OnSelect(data);
        InteractionLogger.StartTimerFor("Playground");
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("Playground");
    }

    public override void onClose()
    {
        SaveAndLoadModule.playgrounds.Remove(this);
        InteractionLogger.Discount("Playground");
        Destroy(gameObject);
    }

    public override void innerBehaviour()
    {
        if (field.isFocused)
        {
            if (Input.anyKeyDown && !loadingWheel.activeSelf)
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
                //else if (((leftCmd || leftCtrl) && b) || f8)
                //    PharoBrowse();
                else if (((leftCmd || leftCtrl) && v) || ((leftCmd || leftCtrl) && c))
                    onChangeInput();
                else
                    onChangeInput();
            }
            lastCaretPosition = field.caretPosition;
        }
    }

    async Task<String> TryGetImageFile(MatchCollection matches, string type, string selectedCode)
    {
        string var = matches[0].Groups[1].Value;
        string lowerType = type.ToLower();
        string exporter;
        if (selectedCode.Contains("RS"))
            exporter =
                ". " + var + " canvas " + lowerType + "Exporter " +
                    "noFixedShapes; " +
                    "fileName: 'img." + lowerType + "'; " +
                    "export. ";
        else
            exporter =
                ". RT" + type + "Exporter new " +
                    (type == "PNG" ? "builder" : "view") + ": (" + var + " view); " +
                    "fileName: 'img." + lowerType + "'; " +
                    "exportToFile. ";

        string finalCode =
                exporter +
                "(FileLocator workingDirectory / 'img." + type.ToLower() + "') " +
                    "binaryReadStreamDo:[ :stream | stream upToEnd ].";

        selectedCode = Regex.Replace(
            selectedCode, 
            $@"(\.)([\n\t\s]*)(\^)?([\n\t\s]*){var}([\n\t\s]+view)?([\n\t\s]*)(\.|\Z)", 
            finalCode
        );

        string res = await Pharo.Print(selectedCode);
        return res;
    }
}