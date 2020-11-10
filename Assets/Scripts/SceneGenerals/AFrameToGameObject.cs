using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using TMPro;
using HTC.UnityPlugin.Pointer3D;

namespace AFrameModule
{
    public static class AFrameToGameObject
    {
        public static async Task<(GameObject canvas, float width, float height)> Convert(String aframe)
        {
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

            float width = Math.Abs(maxX - minX);
            float height = Math.Abs(maxY - minY);

            aFrameCanvas.GetComponent<RectTransform>().sizeDelta = new Vector2(width, height);
            aFramePanel.GetComponent<RectTransform>().sizeDelta = new Vector2(width, height);

            return (aFrameCanvas, width, height);
        }

        private static Vector3 GetNearestObjectToOrigin(string aFrame)
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

        private static void MakeGeometryInteractable(GameObject ob)
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

        private static void MakeGeometryDraggable(GameObject ob)
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
}