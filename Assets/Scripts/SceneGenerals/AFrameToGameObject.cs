using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using TMPro;
using HTC.UnityPlugin.Pointer3D;

namespace AFrameModule
{
    public static class AFrameToGameObject
    {
        public static (GameObject canvas, float width, float height) Convert(String aframe)
        {
            aframe = aframe.Split(new string[] { "</html>" }, StringSplitOptions.None)[0];
            MatchCollection texts = Regex.Matches(aframe, "<a-entity(.*)text=\"(.*)\"(.*)>");
            MatchCollection geometries = Regex.Matches(aframe, "<a-entity(.*)geometry=\"(.*)\"(.*)>");
            MatchCollection lines = Regex.Matches(aframe, "<a-entity(.*)line=\"(.*)\"(.*)>");
            MatchCollection boxes = Regex.Matches(aframe, "<a-box(.*)>");
            MatchCollection spheres = Regex.Matches(aframe, "<a-sphere(.*)>");
            MatchCollection cylinders = Regex.Matches(aframe, "<a-cylinder(.*)>");
            MatchCollection planes = Regex.Matches(aframe, "<a-plane(.*)>");

            Dictionary<Vector3, GameObject> geoMapping = new Dictionary<Vector3, GameObject>();

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
                string tag = m.Value;
                string value = Regex.Match(tag, @"value: ([a-zA-Z0-9-.\s]+)(;|"")").Groups[1].Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                Match widthMatch = widthMatch = Regex.Match(tag, @"width: ([0-9-.]+)(;|"")");
                Match colorMatch = colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");

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

                string primitive = Regex.Match(tag, @"primitive: ([a-zA-Z]+)(;|"")").Groups[1].Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                Match widthMatch = Regex.Match(tag, @"width: ([0-9.]+) ;");
                Match heightMatch = Regex.Match(tag, @"height: ([0-9.]+);");
                Match depthMatch = Regex.Match(tag, @"depth: ([0-9.]+);");
                Match colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                Match metalMatch = Regex.Match(tag, @"metalness: ([0-9.]+)(;|"")");
                Match glossMatch = Regex.Match(tag, @"roughness: ([0-9.]+)(;|"")");

                GameObject ob;
                switch (primitive)
                {
                    case "cube":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                        break;
                    case "cylinder":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
                        break;
                    case "capsule":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Capsule);
                        break;
                    case "plane":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Plane);
                        break;
                    case "quad":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Quad);
                        break;
                    case "sphere":
                        ob = GameObject.CreatePrimitive(PrimitiveType.Sphere);
                        break;
                    default:
                        ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                        break;
                }
                ob.tag = "AFrame";
                ob.AddComponent<AFrameGeometry>();
                ob.AddComponent<EventTrigger>();
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

                // Adding Drag functions
                EventTrigger trigger = ob.GetComponent<EventTrigger>();
                AFrameGeometry afg = ob.GetComponent<AFrameGeometry>();

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

            foreach (Match m in boxes)
            {
                string tag = m.Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                Match colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                Match widthMatch = Regex.Match(tag, @"width=""([0-9.]+)""");
                Match heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                Match depthMatch = Regex.Match(tag, @"depth=""([0-9.]+)""");
                Match metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                Match glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");

                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                ob.tag = "AFrame";
                ob.AddComponent<AFrameGeometry>();
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
            }

            foreach (Match m in spheres)
            {
                string tag = m.Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                Match metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                Match glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                Match radiusMatch = Regex.Match(tag, @"radius=""([0-9.]+)""");

                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                ob.tag = "AFrame";
                ob.AddComponent<AFrameGeometry>();
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

                if(colorMatch.Success || glossMatch.Success || metalMatch.Success)
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
            }

            foreach (Match m in cylinders)
            {
                string tag = m.Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                Match metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                Match glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");
                Match radiusMatch = Regex.Match(tag, @"radius=""([0-9.]+)""");
                Match heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");

                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                ob.tag = "AFrame";
                ob.AddComponent<AFrameGeometry>();
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
            }

            foreach (Match m in planes)
            {
                string tag = m.Value;
                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                Match colorMatch = Regex.Match(tag, @"color=""([#0-9A-Z]+)""");
                Match widthMatch = Regex.Match(tag, @"width=""([0-9.]+)""");
                Match heightMatch = Regex.Match(tag, @"height=""([0-9.]+)""");
                Match metalMatch = Regex.Match(tag, @"metalness=""([0-9.]+)""");
                Match glossMatch = Regex.Match(tag, @"roughness=""([0-9.]+)""");

                GameObject ob = GameObject.CreatePrimitive(PrimitiveType.Cube);
                ob.tag = "AFrame";
                ob.AddComponent<AFrameGeometry>();
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
            }

            foreach (Match m in lines)
            {
                string tag = m.Value;
                Match startMatch = Regex.Match(tag, @"start: ([0-9-.\s]+)(;|"")");
                Match endMatch = Regex.Match(tag, @"end: ([0-9-.\s]+)(;|"")");
                Match colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");

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

                    Vector3 start = new Vector3(s[0], s[1], s[2]) - pivot;
                    Vector3 end = new Vector3(e[0], e[1], e[2]) - pivot;

                    AFrameLine afl = line.GetComponent<AFrameLine>();
                    afl.start = aFramePanel.transform.TransformPoint(start);
                    afl.end = aFramePanel.transform.TransformPoint(end);

                    if (geoMapping.ContainsKey(start))
                         geoMapping.TryGetValue(start, out afl.startObject);

                    if (geoMapping.ContainsKey(end))
                         geoMapping.TryGetValue(end, out afl.endObject);
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

            foreach(Match m in c)
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
    }
}
