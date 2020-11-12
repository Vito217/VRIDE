using System;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using System.Globalization;
using System.Collections.Generic;
using HTC.UnityPlugin.Pointer3D;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using PharoModule;
using LoggingModule;
using SaveAndLoad;
using ImageUtils;
using TMPro;
using System.Collections;

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
                            string aframe = res;

                            MatchCollection texts = null, geometries = null, lines = null, boxes = null,
                                            spheres = null, cylinders = null, planes = null;

                            await Task.Run(() => {
                                aframe = aframe.Split(new string[] { "</html>" }, StringSplitOptions.None)[0];
                                texts = Regex.Matches(aframe, "<a-entity(.*)text=\"(.*)\"(.*)>");
                                geometries = Regex.Matches(aframe, "<a-entity(.*)geometry=\"(.*)\"(.*)>");
                                lines = Regex.Matches(aframe, "<a-entity(.*)line=\"(.*)\"(.*)>");
                                boxes = Regex.Matches(aframe, "<a-box(.*)>");
                                spheres = Regex.Matches(aframe, "<a-sphere(.*)>");
                                cylinders = Regex.Matches(aframe, "<a-cylinder(.*)>");
                                planes = Regex.Matches(aframe, "<a-plane(.*)>");
                            });

                            Dictionary<Vector3, GameObject> geoMapping = new Dictionary<Vector3, GameObject>();
                            Dictionary<Vector3, GameObject> lineMapping = new Dictionary<Vector3, GameObject>();

                            Vector3 pivot = GetNearestObjectToOrigin(aframe);
                            float maxX = float.MinValue;
                            float maxY = float.MinValue;
                            float minX = float.MaxValue;
                            float minY = float.MaxValue;

                            // Base canvas
                            GameObject aFrameCanvas = Instantiator.Instance.AFrame();
                            GameObject aFramePanel = aFrameCanvas.transform.Find("Panel").gameObject;

                            foreach (Match m in texts)
                            {
                                string tag = "", value = "";
                                Match posMatch = null, rotMatch = null, widthMatch = null, colorMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    value = Regex.Match(tag, @"value: ([a-zA-Z0-9-.\s]+)(;|"")").Groups[1].Value;
                                    posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                    rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                    widthMatch = widthMatch = Regex.Match(tag, @"width: ([0-9-.]+)(;|"")");
                                    colorMatch = colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                                });

                                GameObject text = new GameObject(
                                    value,
                                    typeof(TextMeshProUGUI),
                                    typeof(ContentSizeFitter));
                                text.tag = "AFrame";
                                text.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    text.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (rotMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        rotMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    text.transform.localRotation =
                                        Quaternion.Euler(coords[0], coords[1], coords[2]);
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

                                // Update width and height
                                Vector2 size = text.GetComponent<RectTransform>().sizeDelta;

                                maxX = Math.Max(maxX, text.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, text.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, text.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, text.transform.localPosition.y - size.y);
                            }

                            foreach (Match m in geometries)
                            {
                                string tag = m.Value;
                                if (tag.Contains("src: #floor;"))
                                    continue;

                                string primitive = "";
                                Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null,
                                    depthMatch = null, colorMatch = null, metalMatch = null, glossMatch = null;

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
                                if (primitive.Equals("box"))
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                                else if (primitive.Equals("cylinder"))
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
                                else if (primitive.Equals("capsule"))
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Capsule);
                                else if (primitive.Equals("plane"))
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Plane);
                                else if (primitive.Equals("sphere"))
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                                else
                                    ob = GameObject.CreatePrimitive(PrimitiveType.Quad);

                                ob.tag = "AFrame";
                                MakeGeometryInteractable(ob);
                                ob.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (rotMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        rotMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localRotation =
                                        Quaternion.Euler(coords[0], coords[1], coords[2]);
                                }

                                if (widthMatch.Success && heightMatch.Success && depthMatch.Success)
                                {
                                    Vector3 scale = new Vector3(
                                        float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                        float.Parse(heightMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                        float.Parse(depthMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                                    ob.transform.localScale = scale;
                                }

                                if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
                                {
                                    Material material = ob.GetComponent<Renderer>().material;

                                    if (colorMatch.Success)
                                    {
                                        Color c;
                                        ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                        material.color = c;
                                    }

                                    if (glossMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Glossiness",
                                            1f - float.Parse(glossMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }

                                    if (metalMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Metallic",
                                            float.Parse(metalMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }
                                }

                                // Update width and height
                                Vector3 size = ob.transform.localScale;

                                maxX = Math.Max(maxX, ob.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, ob.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, ob.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, ob.transform.localPosition.y - size.y);

                                if (!geoMapping.ContainsKey(ob.transform.localPosition))
                                    geoMapping.Add(ob.transform.localPosition, ob);

                                MakeGeometryDraggable(ob);
                            }

                            foreach (Match m in boxes)
                            {
                                string tag = "";
                                Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null,
                                    depthMatch = null, colorMatch = null, metalMatch = null, glossMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                    rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                    colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                                    widthMatch = Regex.Match(tag, @"width=""([0-9.]+)""");
                                    heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                                    depthMatch = Regex.Match(tag, @"depth=""([0-9.]+)""");
                                    metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                                    glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                                });

                                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                                ob.tag = "AFrame";
                                MakeGeometryInteractable(ob);
                                ob.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (rotMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        rotMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localRotation =
                                        Quaternion.Euler(coords[0], coords[1], coords[2]);
                                }

                                if (widthMatch.Success && heightMatch.Success && depthMatch.Success)
                                {
                                    Vector3 scale = new Vector3(
                                        float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                        float.Parse(heightMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                        float.Parse(depthMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                                    ob.transform.localScale = scale;
                                }

                                if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
                                {
                                    Material material = ob.GetComponent<Renderer>().material;

                                    if (colorMatch.Success)
                                    {
                                        Color c;
                                        ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                        material.color = c;
                                    }

                                    if (glossMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Glossiness",
                                            1f - float.Parse(glossMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }

                                    if (metalMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Metallic",
                                            float.Parse(metalMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }
                                }

                                // Update width and height
                                Vector3 size = ob.transform.localScale;

                                maxX = Math.Max(maxX, ob.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, ob.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, ob.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, ob.transform.localPosition.y - size.y);

                                if (!geoMapping.ContainsKey(ob.transform.localPosition))
                                    geoMapping.Add(ob.transform.localPosition, ob);

                                MakeGeometryDraggable(ob);
                            }

                            foreach (Match m in spheres)
                            {
                                string tag = "";
                                Match posMatch = null, colorMatch = null, metalMatch = null,
                                    glossMatch = null, radiusMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                    colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                                    metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                                    glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                                    radiusMatch = Regex.Match(tag, @"radius=""([0-9.]+)""");
                                });

                                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                                ob.tag = "AFrame";
                                MakeGeometryInteractable(ob);
                                ob.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (radiusMatch.Success)
                                {
                                    float radius = float.Parse(
                                        radiusMatch.Groups[1].Value,
                                        CultureInfo.InvariantCulture);

                                    Vector3 scale = new Vector3(radius, radius, radius);
                                    ob.transform.localScale = scale;
                                }

                                if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
                                {
                                    Material material = ob.GetComponent<Renderer>().material;

                                    if (colorMatch.Success)
                                    {
                                        Color c;
                                        ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                        material.color = c;
                                    }

                                    if (glossMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Glossiness",
                                            1f - float.Parse(glossMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }

                                    if (metalMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Metallic",
                                            float.Parse(metalMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }
                                }

                                // Update width and height
                                Vector3 size = ob.transform.localScale;

                                maxX = Math.Max(maxX, ob.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, ob.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, ob.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, ob.transform.localPosition.y - size.y);

                                if (!geoMapping.ContainsKey(ob.transform.localPosition))
                                    geoMapping.Add(ob.transform.localPosition, ob);

                                MakeGeometryDraggable(ob);
                            }

                            foreach (Match m in cylinders)
                            {
                                string tag = "";
                                Match posMatch = null, colorMatch = null, metalMatch = null, glossMatch = null,
                                    radiusMatch = null, heightMatch = null, rotMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                    colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                                    metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                                    glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                                    radiusMatch = Regex.Match(tag, @"radius=""([0-9.]+)""");
                                    heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                                    rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                });

                                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
                                ob.tag = "AFrame";
                                MakeGeometryInteractable(ob);
                                ob.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (rotMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        rotMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localRotation =
                                        Quaternion.Euler(coords[0], coords[1], coords[2]);
                                }

                                if (radiusMatch.Success && heightMatch.Success)
                                {
                                    float radius = float.Parse(
                                        radiusMatch.Groups[1].Value,
                                        CultureInfo.InvariantCulture);

                                    float h = float.Parse(
                                        heightMatch.Groups[1].Value,
                                        CultureInfo.InvariantCulture);

                                    Vector3 scale = new Vector3(radius, h, radius);
                                    ob.transform.localScale = scale;
                                }

                                if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
                                {
                                    Material material = ob.GetComponent<Renderer>().material;

                                    if (colorMatch.Success)
                                    {
                                        Color c;
                                        ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                        material.color = c;
                                    }

                                    if (glossMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Glossiness",
                                            1f - float.Parse(glossMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }

                                    if (metalMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Metallic",
                                            float.Parse(metalMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }
                                }

                                // Update width and height
                                Vector3 size = ob.transform.localScale;

                                maxX = Math.Max(maxX, ob.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, ob.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, ob.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, ob.transform.localPosition.y - size.y);

                                if (!geoMapping.ContainsKey(ob.transform.localPosition))
                                    geoMapping.Add(ob.transform.localPosition, ob);

                                MakeGeometryDraggable(ob);
                            }

                            foreach (Match m in planes)
                            {
                                string tag = "";
                                Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null,
                                    colorMatch = null, metalMatch = null, glossMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                                    rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                                    colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                                    widthMatch = Regex.Match(tag, @"width=""([0-9.]+)""");
                                    heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                                    metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                                    glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                                });

                                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Plane);
                                ob.tag = "AFrame";
                                MakeGeometryInteractable(ob);
                                ob.transform.SetParent(aFramePanel.transform, false);

                                if (posMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        posMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localPosition =
                                        new Vector3(coords[0], coords[1], coords[2]) - pivot;
                                }

                                if (rotMatch.Success)
                                {
                                    float[] coords = Array.ConvertAll(
                                        rotMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    ob.transform.localRotation =
                                        Quaternion.Euler(coords[0], coords[1], coords[2]);
                                }

                                if (widthMatch.Success && heightMatch.Success)
                                {
                                    Vector3 scale = new Vector3(
                                        float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                                        ob.transform.localScale.y,
                                        float.Parse(heightMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                                    ob.transform.localScale = scale;
                                }

                                if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
                                {
                                    Material material = ob.GetComponent<Renderer>().material;

                                    if (colorMatch.Success)
                                    {
                                        Color c;
                                        ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                        material.color = c;
                                    }

                                    if (glossMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Glossiness",
                                            1f - float.Parse(glossMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }

                                    if (metalMatch.Success)
                                    {
                                        material.SetFloat(
                                            "_Metallic",
                                            float.Parse(metalMatch.Groups[1].Value,
                                            CultureInfo.InvariantCulture));
                                    }
                                }

                                // Update width and height
                                Vector3 size = ob.transform.localScale;

                                maxX = Math.Max(maxX, ob.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, ob.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, ob.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, ob.transform.localPosition.y - size.y);

                                if (!geoMapping.ContainsKey(ob.transform.localPosition))
                                    geoMapping.Add(ob.transform.localPosition, ob);

                                MakeGeometryDraggable(ob);
                            }

                            foreach (Match m in lines)
                            {
                                string tag = "";
                                Match startMatch = null, endMatch = null, colorMatch = null;

                                await Task.Run(() => {
                                    tag = m.Value;
                                    startMatch = Regex.Match(tag, @"start: ([0-9-.\s]+)(;|"")");
                                    endMatch = Regex.Match(tag, @"end: ([0-9-.\s]+)(;|"")");
                                    colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                                });

                                GameObject line = GameObject.CreatePrimitive(PrimitiveType.Cube);
                                line.name = "Line";
                                line.tag = "AFrame";
                                line.AddComponent<AFrameLine>();
                                line.transform.SetParent(aFramePanel.transform, false);

                                if (startMatch.Success && endMatch.Success)
                                {
                                    float[] s = Array.ConvertAll(
                                        startMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    float[] e = Array.ConvertAll(
                                        endMatch.Groups[1].Value.Split(' '),
                                        i => float.Parse(i, CultureInfo.InvariantCulture));

                                    // Local positions
                                    Vector3 start = new Vector3(s[0], s[1], s[2]) - pivot;
                                    Vector3 end = new Vector3(e[0], e[1], e[2]) - pivot;

                                    AFrameLine afl = line.GetComponent<AFrameLine>();
                                    afl.start = start;
                                    afl.end = end;

                                    // Mapping intersections with geometries

                                    // If there is a geometry at the local start point
                                    // Use it as the start object
                                    if (geoMapping.ContainsKey(start))
                                        geoMapping.TryGetValue(start, out afl.startObject);
                                    else
                                    {
                                        // If not
                                        // We check if there is another line whose start/end point
                                        // overlaps its start
                                        if (lineMapping.ContainsKey(start))
                                        {
                                            // It is connected to another line.
                                            // Then, we create a node
                                            GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                                            ob.tag = "AFrame";
                                            MakeGeometryInteractable(ob);
                                            ob.transform.SetParent(aFramePanel.transform, false);
                                            ob.transform.localScale = new Vector3(.06f, .06f, .06f);
                                            ob.transform.localPosition = afl.start;
                                            geoMapping.Add(start, ob);
                                            MakeGeometryDraggable(ob);

                                            // Both lines must share the same node
                                            GameObject intersec;
                                            lineMapping.TryGetValue(start, out intersec);
                                            AFrameLine nearLine = intersec.GetComponent<AFrameLine>();
                                            afl.startObject = ob;
                                            if (nearLine.start == afl.start)
                                                nearLine.startObject = ob;
                                            else
                                                nearLine.endObject = ob;
                                        }
                                        else
                                            lineMapping.Add(start, line);
                                    }

                                    if (geoMapping.ContainsKey(end))
                                        geoMapping.TryGetValue(end, out afl.endObject);
                                    else
                                    {
                                        if (lineMapping.ContainsKey(end))
                                        {
                                            // It is connected to another line
                                            GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                                            ob.tag = "AFrame";
                                            MakeGeometryInteractable(ob);
                                            ob.transform.SetParent(aFramePanel.transform, false);
                                            ob.transform.localScale = new Vector3(.06f, .06f, .06f);
                                            ob.transform.localPosition = afl.end;
                                            geoMapping.Add(end, ob);
                                            MakeGeometryDraggable(ob);

                                            // Both lines must share the same node
                                            GameObject intersec;
                                            lineMapping.TryGetValue(end, out intersec);
                                            AFrameLine nearLine = intersec.GetComponent<AFrameLine>();
                                            afl.endObject = ob;
                                            if (nearLine.start == afl.end)
                                                nearLine.startObject = ob;
                                            else
                                                nearLine.endObject = ob;
                                        }
                                        else
                                            lineMapping.Add(end, line);
                                    }
                                }

                                if (colorMatch.Success)
                                {
                                    Color c;
                                    ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                                    Material material = line.GetComponent<Renderer>().material;
                                    material.color = c;
                                }

                                // Update width and height
                                Vector3 size = line.transform.localScale;

                                maxX = Math.Max(maxX, line.transform.localPosition.x + size.x);
                                maxY = Math.Max(maxY, line.transform.localPosition.y + size.y);
                                minX = Math.Min(minX, line.transform.localPosition.x - size.x);
                                minY = Math.Min(minY, line.transform.localPosition.y - size.y);
                            }

                            float finalWidth = Math.Abs(maxX - minX);
                            float finalHeight = Math.Abs(maxY - minY);

                            aFrameCanvas.GetComponent<RectTransform>().sizeDelta = new Vector2(finalWidth, finalHeight);
                            aFramePanel.GetComponent<RectTransform>().sizeDelta = new Vector2(finalWidth, finalHeight);

                            float finalScale = (GetComponent<RectTransform>().sizeDelta.y / finalHeight) * 0.001f;
                            float finalToolbarScale = 0.001f / finalScale;
                            aFrameCanvas.transform.localScale = new Vector3(finalScale, finalScale, finalScale);
                            aFrameCanvas.transform.Find("Panel/Toolbar").localScale = 
                                new Vector3(finalToolbarScale, finalToolbarScale, finalToolbarScale);

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
            string selection = getSelectedCode(field.text, false);
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
        keyboardTarget = data.selectedObject.GetComponent<TMP_InputField>();
        wordPicker.TextField = keyboardTarget;
        InteractionLogger.StartTimerFor("Playground");
    }

    public override void OnDeselect(BaseEventData data)
    {
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
            if (fromUIClick)
            {
                fromUIClick = false;
                keyboardTarget.caretPosition = lastCaretPosition;
                keyboardTarget.selectionAnchorPosition = lastAnchorPosition;
            }

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

    private Vector3 GetNearestObjectToOrigin(string aFrame)
    {
        float minMagnitude = float.MaxValue;
        Vector3 pivot = Vector3.zero;
        MatchCollection c = Regex.Matches(aFrame,
            @"(start|end|position)(:\s|="")([0-9-.\s]+)(;|"")");

        foreach (Match m in c)
        {
            float[] coords =
                Array.ConvertAll(
                    m.Groups[3].Value.Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

            Vector3 pos = new Vector3(coords[0], coords[1], coords[2]);
            if (pos.sqrMagnitude < minMagnitude && pos != Vector3.zero)
            {
                minMagnitude = pos.sqrMagnitude;
                pivot = pos;
            }
        }
        return pivot;
    }

    private void MakeGeometryInteractable(GameObject ob)
    {
        ob.AddComponent<EventTrigger>();
        ob.AddComponent<GraphicRaycaster>();
        ob.AddComponent<CanvasRaycastTarget>();
        ob.AddComponent<InitializeBehaviour>();
        ob.GetComponent<RectTransform>().pivot = Vector2.zero;
        ob.GetComponent<RectTransform>().anchorMin = Vector2.zero;
        ob.GetComponent<RectTransform>().anchorMax = Vector2.zero;
        ob.GetComponent<RectTransform>().sizeDelta = Vector2.zero;
        ob.GetComponent<InitializeBehaviour>().freezeRotation = false;
    }

    private void MakeGeometryDraggable(GameObject ob)
    {
        // Adding Drag functions
        EventTrigger trigger = ob.GetComponent<EventTrigger>();
        InitializeBehaviour afg = ob.GetComponent<InitializeBehaviour>();

        // OnPointerDown -> OnDrag
        EventTrigger.Entry entry = new EventTrigger.Entry();
        entry.eventID = EventTriggerType.PointerDown;
        entry.callback.AddListener((data) => { afg.OnDrag(data); });
        trigger.triggers.Add(entry);

        // OnPointerUp -> OnEndDrag
        EventTrigger.Entry entryTwo = new EventTrigger.Entry();
        entryTwo.eventID = EventTriggerType.PointerUp;
        entryTwo.callback.AddListener((data) => { afg.OnEndDrag(data); });
        trigger.triggers.Add(entryTwo);
    }
}