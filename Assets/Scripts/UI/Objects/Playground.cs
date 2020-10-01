using System;
using System.Threading.Tasks;
using System.IO;
using System.Globalization;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using HTC.UnityPlugin.Pointer3D;
using UnityEngine.EventSystems;
using PharoModule;
using LoggingModule;
using SaveAndLoad;
using ImageUtils;
using TMPro;
using AFrameModule;

public class Playground : InitializeBehaviour
{
    private Graph view;
    private Inspector insp;
    public AutocompleteWordPicker wordPicker;

    public async void PharoDo()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selectedCode = getSelectedCode(cleanCode(field.text), false);

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
                string res = "";
                Match match = matches[0];
                if (match.Value.Contains("RS"))
                {
                    res = await TryGetImageFile(match, "aFrame", selectedCode);
                    string resPNG = await TryGetImageFile(match, "png", selectedCode);
                    if (res.Contains("<html>"))
                    {
                        string aframe = "";
                        MatchCollection texts = null;
                        MatchCollection geometries = null;

                        await Task.Run(() => {
                            aframe = res.Split(new string[] { "</html>" }, StringSplitOptions.None)[0];
                            texts = Regex.Matches(aframe, "<a-entity(.*)text=\"(.*)\" >");
                            geometries = Regex.Matches(aframe, "<a-entity(.*)geometry=\"(.*)\" >");
                        });

                        // Base canvas
                        GameObject aFrameCanvas = new GameObject("AFrame", typeof(Canvas), typeof(CanvasScaler),
                            typeof(GraphicRaycaster), typeof(CanvasRaycastTarget),
                            typeof(InitializeBehaviour), typeof(EventTrigger));
                        aFrameCanvas.GetComponent<Canvas>().renderMode = RenderMode.WorldSpace;
                        aFrameCanvas.GetComponent<RectTransform>().sizeDelta = new Vector2(2f, 2f);
                        aFrameCanvas.transform.localScale = new Vector3(.2f, .2f, .2f); ;

                        GameObject aFramePanel = new GameObject("Panel", typeof(RectTransform), typeof(CanvasRenderer));
                        aFramePanel.transform.SetParent(aFrameCanvas.transform, false);

                        foreach (Match m in texts)
                        {
                            string tag = m.Value;
                            string value = "";
                            Match posMatch = null;
                            Match rotMatch = null;
                            Match widthMatch = null;
                            Match colorMatch = null;

                            await Task.Run(() => {
                                value = Regex.Match(tag, @"value: ([a-zA-Z0-9-.\s]+)(;|"")").Groups[1].Value;
                                posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                widthMatch = Regex.Match(tag, @"width: ([0-9-.]+)(;|"")");
                                colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                            });
                            
                            // A text object
                            GameObject text = new GameObject(
                                value,
                                typeof(TextMeshProUGUI),
                                typeof(ContentSizeFitter));
                            text.transform.SetParent(aFramePanel.transform, false);

                            if (posMatch.Success)
                            {
                                string[] p = posMatch.Groups[1].Value.Split(' ');
                                Vector3 position = new Vector3(
                                    float.Parse(p[0], CultureInfo.InvariantCulture),
                                    float.Parse(p[1], CultureInfo.InvariantCulture),
                                    float.Parse(p[2], CultureInfo.InvariantCulture));
                                text.transform.localPosition = position;
                            }

                            if (rotMatch.Success)
                            {
                                string[] r = rotMatch.Groups[1].Value.Split(' ');
                                Quaternion rotation = Quaternion.Euler(
                                    float.Parse(r[0], CultureInfo.InvariantCulture),
                                    float.Parse(r[1], CultureInfo.InvariantCulture),
                                    float.Parse(r[2], CultureInfo.InvariantCulture));
                                text.transform.localRotation = rotation;
                            }

                            ContentSizeFitter fitter = text.GetComponent<ContentSizeFitter>();
                            fitter.verticalFit = ContentSizeFitter.FitMode.PreferredSize;

                            if (widthMatch.Success)
                            {
                                var sd = text.GetComponent<RectTransform>().sizeDelta;
                                sd.x = float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture);
                                text.GetComponent<RectTransform>().sizeDelta = sd;
                            }

                            if (colorMatch.Success)
                            {
                                TextMeshProUGUI textComponent = text.GetComponent<TextMeshProUGUI>();
                                textComponent.text =
                                    "<size=0.1><color=" + colorMatch.Groups[1].Value + ">" + value + "</color></size>";
                            }

                        }

                        foreach (Match m in geometries)
                        {
                            string tag = m.Value;
                            string primitive = "";
                            Match posMatch = null;
                            Match rotMatch = null;
                            Match widthMatch = null;
                            Match heightMatch = null;
                            Match depthMatch = null;
                            Match colorMatch = null;
                            Match metalMatch = null;
                            Match glossMatch = null;

                            await Task.Run(() => {
                                primitive = Regex.Match(tag, @"primitive: ([a-zA-Z]+)(;|"")").Groups[1].Value;
                                posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                widthMatch = Regex.Match(tag, @"width: ([0-9.]+) ;");
                                heightMatch = Regex.Match(tag, @"height: ([0-9.]+);");
                                depthMatch = Regex.Match(tag, @"depth: ([0-9.]+);");
                                colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                                metalMatch = Regex.Match(tag, @"metalness: ([0-9.]+)(;|"")");
                                glossMatch = Regex.Match(tag, @"roughness: ([0-9.]+)(;|"")");
                            });

                            GameObject ob;
                            switch (primitive)
                            {
                                case "cube":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                                    UnityEngine.Object.Destroy(ob.GetComponent<BoxCollider>());
                                    break;
                                case "cylinder":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
                                    UnityEngine.Object.Destroy(ob.GetComponent<CapsuleCollider>());
                                    break;
                                case "capsule":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Capsule);
                                    UnityEngine.Object.Destroy(ob.GetComponent<CapsuleCollider>());
                                    break;
                                case "plane":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Plane);
                                    UnityEngine.Object.Destroy(ob.GetComponent<MeshCollider>());
                                    break;
                                case "quad":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Quad);
                                    UnityEngine.Object.Destroy(ob.GetComponent<MeshCollider>());
                                    break;
                                case "sphere":
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                                    UnityEngine.Object.Destroy(ob.GetComponent<SphereCollider>());
                                    break;
                                default:
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                                    UnityEngine.Object.Destroy(ob.GetComponent<BoxCollider>());
                                    break;
                            }
                            ob.AddComponent<RectTransform>();
                            ob.AddComponent<CanvasRenderer>();
                            ob.transform.SetParent(aFramePanel.transform, false);

                            if (posMatch.Success)
                            {
                                string[] p = posMatch.Groups[1].Value.Split(' ');
                                Vector3 position = new Vector3(
                                    float.Parse(p[0], CultureInfo.InvariantCulture),
                                    float.Parse(p[1], CultureInfo.InvariantCulture),
                                    float.Parse(p[2], CultureInfo.InvariantCulture));
                                ob.transform.localPosition = position;
                            }

                            if (rotMatch.Success)
                            {
                                string[] r = rotMatch.Groups[1].Value.Split(' ');
                                Quaternion rotation = Quaternion.Euler(
                                    float.Parse(r[0], CultureInfo.InvariantCulture),
                                    float.Parse(r[1], CultureInfo.InvariantCulture),
                                    float.Parse(r[2], CultureInfo.InvariantCulture));
                                ob.transform.localRotation = rotation;
                            }

                            if (widthMatch.Success)
                            {
                                Vector3 scale = new Vector3(
                                    float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                    float.Parse(heightMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                    float.Parse(depthMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                                ob.transform.localScale = scale;
                            }

                            if (colorMatch.Success)
                            {
                                Color c;
                                ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                Material material = ob.GetComponent<Renderer>().material;
                                material.color = c;
                                material.SetFloat("_Glossiness", 1f - float.Parse(glossMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                                material.SetFloat("_Metallic", float.Parse(metalMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                            }
                        }

                        aFramePanel.transform.localPosition = new Vector3(-0.5f, -1f, 0f);

                        // Adding Drag functions
                        EventTrigger trigger = aFrameCanvas.GetComponent<EventTrigger>();
                        InitializeBehaviour ib = aFrameCanvas.GetComponent<InitializeBehaviour>();

                        // OnPointerDown -> OnDrag
                        EventTrigger.Entry entry = new EventTrigger.Entry();
                        entry.eventID = EventTriggerType.PointerDown;
                        entry.callback.AddListener((data) => { ib.OnDrag(data); });
                        trigger.triggers.Add(entry);

                        // OnPointerUp -> OnEndDrag
                        EventTrigger.Entry entryTwo = new EventTrigger.Entry();
                        entryTwo.eventID = EventTriggerType.PointerUp;
                        entryTwo.callback.AddListener((data) => { ib.OnEndDrag(data); });
                        trigger.triggers.Add(entryTwo);

                        // Setting Mixed Shader Channels
                        aFrameCanvas.GetComponent<Canvas>().additionalShaderChannels =
                            AdditionalCanvasShaderChannels.TexCoord1 |
                            AdditionalCanvasShaderChannels.Normal |
                            AdditionalCanvasShaderChannels.Tangent;

                        // Setting width and height that match with its PNG equivalent
                        Sprite tempSp = ImageModule.ImportPNG(resPNG);
                        float pngWidth = tempSp.texture.width;
                        float pngHeight = tempSp.texture.height;
                        var sdlt = aFrameCanvas.GetComponent<RectTransform>().sizeDelta;
                        sdlt.x = pngWidth * sdlt.y / pngHeight;
                        aFrameCanvas.GetComponent<RectTransform>().sizeDelta = sdlt;

                        // Positioning
                        float width = GetComponent<RectTransform>().sizeDelta.x;
                        aFrameCanvas.GetComponent<InitializeBehaviour>().Initialize(
                            transform.TransformPoint(new Vector3(
                                -0.5f * (width + aFrameCanvas.GetComponent<RectTransform>().sizeDelta.x), 0, 0)),
                            transform.forward
                        );
                    }
                    else
                    {
                        try
                        {
                            res = await TryGetImageFile(match, "svg", selectedCode);
                            if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                                GenerateView(res, "SVG", resPNG);
                        }
                        catch
                        {
                            res = resPNG;
                            if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                                GenerateView(res, "PNG", resPNG);
                            else
                                throw new Exception("Couldn't export view.");
                        }
                    }
                }
                else if (match.Value.Contains("RT"))
                {
                    string resPNG = await TryGetImageFile(match, "png", selectedCode);
                    try
                    {
                        res = await TryGetImageFile(match, "svg", selectedCode);
                        if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                            GenerateView(res, "SVG", resPNG);
                    }
                    catch
                    {
                        res = resPNG;
                        if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                            GenerateView(res, "PNG", resPNG);
                        else
                            throw new Exception("Couldn't export view.");
                    }
                }
                else
                {
                    logText.text = 
                        "<color=#C63737>"+res.Remove(res.LastIndexOf("\n"), 1)+"</color>";
                }
                InteractionLogger.RegisterCodeExecution(selectedCode, res);
            }
            else if (!String.IsNullOrWhiteSpace(selectedCode))
            {
                await PharoInspect();
            }
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        Reactivate();
    }

    public async void PharoPrint()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text), false);

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

            logText.text = Regex.Match(res, @"Error|Exception|Notification").Success ?
                "<color=#C63737>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>" :
                res.Remove(res.LastIndexOf("\n"), 1);

            InteractionLogger.RegisterCodeExecution(selection, res);
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        Reactivate();
    }

    public async Task PharoInspect()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selection = getSelectedCode(cleanCode(field.text), false);
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
                logText.text = 
                    "<color=#C63737>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            }

            InteractionLogger.RegisterCodeInspection(selection, res);
        }
        catch (Exception e)
        {
            //output = " <color=#b32d00>[Error] " + e.Message + "</color>";
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
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
        keyboardTarget = data.selectedObject.GetComponent<TMP_InputField>();
        wordPicker.TextField = keyboardTarget;
        InteractionLogger.StartTimerFor("Playground");
    }

    public override void OnDeselect(BaseEventData data)
    {
        base.OnDeselect(data);
        InteractionLogger.EndTimerFor("Playground");
    }

    public override void onClose()
    {
        if (loadingWheel == null || !loadingWheel.activeSelf)
        {
            SaveAndLoadModule.playgrounds.Remove(this);
            InteractionLogger.Discount("Playground");
            Destroy(gameObject);
        }
    }

    public override void innerBehaviour()
    {
        if (field.isFocused || keyboardTarget.isFocused)
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

                //if (!(leftCmd || leftCtrl || f3 || f4 || f5))
                //    onChangeInput();
                if (((leftCmd || leftCtrl) && d) || f3)
                    PharoDo();
                else if (((leftCmd || leftCtrl) && p) || f4)
                    PharoPrint();
                else if (((leftCmd || leftCtrl) && i) || f5)
                    PharoInspect();
                //else if (((leftCmd || leftCtrl) && b) || f8)
                //    PharoBrowse();
                //else if (((leftCmd || leftCtrl) && v) || ((leftCmd || leftCtrl) && c))
                //    onChangeInput();
                //else
                //    onChangeInput();
            }
            lastCaretPosition = keyboardTarget.caretPosition;
        }
    }

    async Task<String> TryGetImageFile(Match match, string type, string selectedCode)
    {
        string var = match.Groups[1].Value;
        string upperType = type.ToUpper();
        string exporter;
        if (selectedCode.Contains("RS"))
            exporter =
                ". " + var + " canvas " + type + "Exporter " +
                    "noFixedShapes; " +
                    "fileName: 'temp." + (type == "aFrame" ? "html" : type) + "'; " +
                    "export. ";
        else
            exporter =
                ". RT" + upperType + "Exporter new " +
                    (type == "png" ? "builder" : "view") + ": (" + var + " view); " +
                    "fileName: 'temp." + (type == "aFrame" ? "html" : type) + "'; " +
                    "exportToFile. ";

        string finalCode =
                exporter +
                "(FileLocator workingDirectory / 'temp." + (type == "aFrame" ? "html" : type) + "') "
                    + (type == "aFrame" ? "r" : "binaryR") + "eadStreamDo:[ :stream | stream upToEnd ].";

        selectedCode = Regex.Replace(
            selectedCode, 
            $@"(\.)([\n\t\s]*)(\^)?([\n\t\s]*){var}([\n\t\s]+view)?([\n\t\s]*)(\.|\Z)", 
            finalCode
        );

        string res = await Pharo.Print(selectedCode);
        return res;
    }

    void GenerateView(string responseString, string type, string rawPNG)
    {
        if (view == null)
        {
            view = Instantiator.Instance.Graph() as Graph;
            SaveAndLoadModule.graphs.Add(view);
            InteractionLogger.Count("GraphObject");
        }
        view.setSprite(responseString, type);

        Sprite tempSp = ImageModule.ImportPNG(rawPNG);
        float pngWidth = tempSp.texture.width;
        float pngHeight = tempSp.texture.height;
        var sd = view.GetComponent<RectTransform>().sizeDelta;
        sd.x = pngWidth * sd.y / pngHeight;
        view.GetComponent<RectTransform>().sizeDelta = sd;

        float width = GetComponent<RectTransform>().sizeDelta.x;
        view.Initialize(
            transform.TransformPoint(new Vector3(
                -0.5f * (width + view.GetComponent<RectTransform>().sizeDelta.x), 0, 0)),
            transform.forward
        );
    }
}