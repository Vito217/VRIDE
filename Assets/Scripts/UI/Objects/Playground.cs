using System;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using PharoModule;
using LoggingModule;
using SaveAndLoad;
using ImageUtils;
using AFrameModule;
using TMPro;

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

            MatchCollection matches = Regex.Matches(selectedCode,
                @"([a-zA-Z0-9]+)([\n\s\t]*)(:=)([\n\s\t]*)((?!Example)((RS|RT).*))([\n\s\t]+)(new)([\n\s\t]*)(\.)");
            if (matches.Count > 0)
            {
                string res = "";
                Match match = matches[0];
                if (match.Value.Contains("RS"))
                {
                    string resPNG = await TryGetImageFile(match, "png", selectedCode);

                    res = await TryGetImageFile(match, "aFrame", selectedCode);
                    if (res.Contains("<html>"))
                    {
                        try
                        {
                            (GameObject g, float w, float h) t = AFrameToGameObject.Convert(res);
                            GameObject aFrameCanvas = t.g;
                            float finalWidth = t.w;
                            float finalHeight = t.h;
                            float finalScale = (GetComponent<RectTransform>().sizeDelta.y / finalHeight) * 0.001f;
                            float finalToolbarScale = 0.001f / finalScale;
                            aFrameCanvas.transform.localScale = new Vector3(finalScale, finalScale, finalScale);
                            aFrameCanvas.transform.Find("Panel/Toolbar").localScale = new Vector3(
                                finalToolbarScale, finalToolbarScale, finalToolbarScale);

                            float scaledWidth = finalWidth * finalScale;
                            float scaledHeight = finalHeight * finalScale;

                            Vector3 aFramePos = transform.TransformPoint(
                                new Vector3(
                                    -.5f * (GetComponent<RectTransform>().sizeDelta.x + scaledWidth),
                                    transform.position.y < .5f * scaledHeight ? 
                                        .5f * scaledHeight - transform.position.y:
                                        0f, 
                                    0f));

                            // Positioning
                            aFrameCanvas.GetComponent<InitializeBehaviour>().Initialize(
                                aFramePos,
                                transform.forward
                            );

                            goto Reactivation;
                        }
                        catch { goto RSToSVG; }
                    }
                    else
                        goto RSToSVG;

                    RSToSVG:
                        res = await TryGetImageFile(match, "svg", selectedCode);
                        if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                        {
                            try 
                            { 
                                GenerateView(res, "SVG", resPNG);
                                goto Reactivation;
                            }
                            catch { goto RSToPNG; }
                        }
                        else
                            goto RSToPNG;

                        RSToPNG:
                            res = resPNG;
                            if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                            {
                                try 
                                { 
                                    GenerateView(res, "PNG", resPNG);
                                    goto Reactivation;
                                }
                                catch { goto RSFailed; }
                            }
                            else
                                goto RSFailed;

                            RSFailed:
                                throw new Exception("Couldn't export view.");
                }
                else if (match.Value.Contains("RT"))
                {
                    string resPNG = await TryGetImageFile(match, "png", selectedCode);
                    
                    res = await TryGetImageFile(match, "svg", selectedCode);
                    if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                    {
                        try 
                        { 
                            GenerateView(res, "SVG", resPNG);
                            goto Reactivation;
                        }
                        catch { goto RTToPNG; }
                    }
                    else
                        goto RTToPNG;

                    RTToPNG:
                        res = resPNG;
                        if (Regex.Match(res, @"\[[0-9\s]+\]").Success)
                        {
                            try 
                            { 
                                GenerateView(res, "PNG", resPNG);
                                goto Reactivation;
                            }
                            catch { goto RTFailed; }
                        }
                        else
                            goto RTFailed;

                        RTFailed:
                            throw new Exception("Couldn't export view.");
                }
                else
                {
                    res = await Pharo.Print(selectedCode);
                    logText.text = 
                        "<color=#C63737>"+res.Remove(res.LastIndexOf("\n"), 1)+"</color>";
                }
            Reactivation:
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
        if (keyboardTarget.isFocused)
        {
            if (Input.anyKeyDown && !loadingWheel.activeSelf)
            {
                bool cmd = Input.GetKey(KeyCode.LeftCommand) ||
                           Input.GetKey(KeyCode.LeftControl) ||
                           Input.GetKey(KeyCode.RightControl);

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

                if ((cmd && d) || f3)
                    PharoDo();
                else if ((cmd && p) || f4)
                    PharoPrint();
                else if ((cmd && i) || f5)
                    PharoInspect();

                //else if (((leftCmd || leftCtrl) && b) || f8)
                //    PharoBrowse();
                //else if (((leftCmd || leftCtrl) && v) || ((leftCmd || leftCtrl) && c))
                //    onChangeInput();
                //else
                //    onChangeInput();
            }
            lastCaretPosition = keyboardTarget.caretPosition;
            lastAnchorPosition = keyboardTarget.selectionAnchorPosition;
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