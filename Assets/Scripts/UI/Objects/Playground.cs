using System;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using PharoModule;
using LoggingModule;
using SaveAndLoad;
using ImageUtils;

public class Playground : InitializeBehaviour
{
    private Graph view;
    private Inspector insp;

    public async void PharoDo()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selectedCode = getSelectedCode(field.text, false);

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
                Match match = matches[0];
                if (match.Value.Contains("RS") || match.Value.Contains("RT"))
                {
                    string resPNG = await TryGetImageFile(match, "png", selectedCode);
                    string resAFrame = await TryGetImageFile(match, "aFrame", selectedCode);
                    string resSVG = await TryGetImageFile(match, "svg", selectedCode);

                    bool aFrameAvailable = resAFrame.Contains("<html>");
                    bool pngAvailable = Regex.Match(resPNG, @"\[[0-9\s]+\]").Success;
                    bool svgAvailable = Regex.Match(resSVG, @"\[[0-9\s]+\]").Success;

                    if (aFrameAvailable)
                    {
                        try
                        {
                            GetComponent<AFrameImporter>().GenerateAFrame(resAFrame);
                            InteractionLogger.RegisterCodeExecution(selectedCode, resAFrame);
                            goto Reactivation;
                        }
                        catch { }
                    }

                    if (svgAvailable)
                    {
                        try
                        {
                            GenerateView(resSVG, "SVG", resPNG);
                            InteractionLogger.RegisterCodeExecution(selectedCode, resSVG);
                            goto Reactivation;
                        }
                        catch { }
                    }

                    if (pngAvailable)
                    {
                        try
                        {
                            GenerateView(resPNG, "PNG", resPNG);
                            InteractionLogger.RegisterCodeExecution(selectedCode, resPNG);
                            goto Reactivation;
                        }
                        catch { }
                    }

                    throw new Exception("Couldn't export view.");
                }
                else
                {
                    string res = await Pharo.Print(selectedCode);
                    logText.text = "<color=#C63737>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
                    InteractionLogger.RegisterCodeExecution(selectedCode, res);
                }
            }
            else if (!String.IsNullOrWhiteSpace(selectedCode))
            {
                await PharoInspect();
            }
        }
        catch (Exception e)
        {
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        Reactivation:
            Reactivate();
    }

    public async void PharoPrint()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selection = getSelectedCode(field.text, false);

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

            logText.text = Regex.Match(res, @"Error|Exception|Notification").Success ?
                "<color=#C63737>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>" :
                res.Remove(res.LastIndexOf("\n"), 1);

            InteractionLogger.RegisterCodeExecution(selection, res);
        }
        catch (Exception e)
        {
            logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        Reactivate();
    }

    public void Inspect()
    {
        _ = PharoInspect();
    }

    public async Task PharoInspect()
    {
        DeactivateTemporarily();
        logText.text = "";
        try
        {
            string selection = getSelectedCode(field.text, false);
            string res = await Pharo.Inspect(selection);
            if (res.Contains("OrderedCollection"))
            {
                if (insp == null)
                {
                    insp = Instantiator.Instance.Inspector();

                    float width = GetComponent<RectTransform>().sizeDelta.x * transform.Find("Panel").GetComponent<RectTransform>().localScale.x;
                    insp.transform.Find("Panel").GetComponent<RectTransform>().localScale = transform.Find("Panel").GetComponent<RectTransform>().localScale;
                    insp.GetComponent<RectTransform>().sizeDelta = GetComponent<RectTransform>().sizeDelta;
                    insp.transform.position = transform.TransformPoint(width, 0f, 0f);
                    insp.transform.forward = transform.forward;

                    SaveAndLoadModule.inspectors.Add(insp);
                    InteractionLogger.Count("Inspector", insp.GetInstanceID().ToString());
                }
                insp.setContent(res);
            }
            else
            {
                logText.text = "<color=#C63737>" + res.Remove(res.LastIndexOf("\n"), 1) + "</color>";
            }

            InteractionLogger.RegisterCodeInspection(selection, res);
        }
        catch (Exception e)
        {
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
        InteractionLogger.StartTimerFor("Playground", GetInstanceID().ToString());
    }

    public override void OnDeselect(BaseEventData data)
    {
        InteractionLogger.EndTimerFor("Playground", GetInstanceID().ToString());
    }

    public override void onClose()
    {
        if (!SomethingIsLoading())
        {
            SaveAndLoadModule.playgrounds.Remove(this);
            InteractionLogger.Discount("Playground", GetInstanceID().ToString());
            Destroy(gameObject);
        }
    }

    public override void innerBehaviour()
    {
        if (field.isFocused)
        {
            if (Input.anyKeyDown && !SomethingIsLoading())
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

                if ((cmd && d) || f3)
                    PharoDo();
                else if ((cmd && p) || f4)
                    PharoPrint();
                else if ((cmd && i) || f5)
                    _ = PharoInspect();
            }
        }
    }

    void LateUpdate()
    {
        CountLines();
        HighlightCode();
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
            InteractionLogger.Count("GraphObject", view.GetInstanceID().ToString());
        }
        view.setSprite(responseString, type);

        Sprite tempSp = ImageModule.ImportPNG(rawPNG);
        float pngWidth = tempSp.texture.width;
        float pngHeight = tempSp.texture.height;
        var sd = view.GetComponent<RectTransform>().sizeDelta;
        sd.x = pngWidth * sd.y / pngHeight;
        view.GetComponent<RectTransform>().sizeDelta = sd;

        float width = GetComponent<RectTransform>().sizeDelta.x;
        view.transform.position = transform.TransformPoint(new Vector3(
                -0.5f * (width + view.GetComponent<RectTransform>().sizeDelta.x), 0, 0));
        view.transform.forward = transform.forward;
    }

    void HighlightCode()
    {

    }
}