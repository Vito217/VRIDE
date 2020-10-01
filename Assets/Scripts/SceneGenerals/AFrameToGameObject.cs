using System;
using System.Globalization;
using System.Linq;
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
        public static GameObject Convert(String aframe)
        {
            aframe = aframe.Split(new string[] { "</html>" }, StringSplitOptions.None)[0];

            // Base canvas
            GameObject canvas = new GameObject("AFrame", typeof(Canvas), typeof(CanvasScaler),
                typeof(GraphicRaycaster), typeof(CanvasRaycastTarget),
                typeof(InitializeBehaviour), typeof(EventTrigger));
            canvas.GetComponent<Canvas>().renderMode = RenderMode.WorldSpace;
            canvas.GetComponent<RectTransform>().sizeDelta = new Vector2(2f, 2f);
            canvas.transform.localScale = new Vector3(.3f, .3f, .3f); ;

            GameObject panel = new GameObject("Panel", typeof(RectTransform), typeof(CanvasRenderer));
            panel.transform.SetParent(canvas.transform, false);

            foreach (Match m in Regex.Matches(aframe,
                "<a-entity(.*)text=\"(.*)\" >"))
            {
                string tag = m.Value;
                string value = Regex.Match(tag, @"value: ([a-zA-Z0-9-.\s]+)(;|"")").Groups[1].Value;

                // A text object
                GameObject text = new GameObject(
                    value, 
                    typeof(TextMeshProUGUI),
                    typeof(ContentSizeFitter));
                text.transform.SetParent(panel.transform, false);

                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                if (posMatch.Success)
                {
                    string[] p = posMatch.Groups[1].Value.Split(' ');
                    Vector3 position = new Vector3(
                        float.Parse(p[0], CultureInfo.InvariantCulture),
                        float.Parse(p[1], CultureInfo.InvariantCulture),
                        float.Parse(p[2], CultureInfo.InvariantCulture));
                    text.transform.localPosition = position;
                }

                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
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

                Match widthMatch = Regex.Match(tag, @"width: ([0-9-.]+)(;|"")");
                if (widthMatch.Success)
                {
                    var sd = text.GetComponent<RectTransform>().sizeDelta;
                    sd.x = float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture);
                    text.GetComponent<RectTransform>().sizeDelta = sd;
                }

                Match colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                if (colorMatch.Success)
                {
                    TextMeshProUGUI textComponent = text.GetComponent<TextMeshProUGUI>();
                    textComponent.text =
                        "<size=0.1><color=" + colorMatch.Groups[1].Value + ">" + value + "</color></size>";
                }
                                           
            }

            foreach (Match m in Regex.Matches(aframe,
                "<a-entity(.*)geometry=\"(.*)\" >"))
            {
                string tag = m.Value;
                string primitive = Regex.Match(tag, @"primitive: ([a-zA-Z]+)(;|"")").Groups[1].Value;

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
                ob.transform.SetParent(panel.transform, false);

                Match posMatch = Regex.Match(tag, @"position=""([0-9-.\s]+)""");
                if (posMatch.Success)
                {
                    string[] p = posMatch.Groups[1].Value.Split(' ');
                    Vector3 position = new Vector3(
                        float.Parse(p[0], CultureInfo.InvariantCulture),
                        float.Parse(p[1], CultureInfo.InvariantCulture),
                        float.Parse(p[2], CultureInfo.InvariantCulture));
                    ob.transform.localPosition = position;
                }

                Match rotMatch = Regex.Match(tag, @"rotation=""([0-9-.\s]+)""");
                if (rotMatch.Success)
                {
                    string[] r = rotMatch.Groups[1].Value.Split(' ');
                    Quaternion rotation = Quaternion.Euler(
                        float.Parse(r[0], CultureInfo.InvariantCulture),
                        float.Parse(r[1], CultureInfo.InvariantCulture),
                        float.Parse(r[2], CultureInfo.InvariantCulture));
                    ob.transform.localRotation = rotation;
                }

                Match widthMatch = Regex.Match(tag, @"width: ([0-9.]+) ;");
                Match heightMatch = Regex.Match(tag, @"height: ([0-9.]+);");
                Match depthMatch = Regex.Match(tag, @"depth: ([0-9.]+);");
                if (widthMatch.Success)
                {
                    Vector3 scale = new Vector3(
                        float.Parse(widthMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                        float.Parse(heightMatch.Groups[1].Value, CultureInfo.InvariantCulture),
                        float.Parse(depthMatch.Groups[1].Value, CultureInfo.InvariantCulture));
                    ob.transform.localScale = scale;
                }

                Match colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");
                Match metalMatch = Regex.Match(tag, @"metalness: ([0-9.]+)(;|"")");
                Match glossMatch = Regex.Match(tag, @"roughness: ([0-9.]+)(;|"")");
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
            /**foreach (Match m in Regex.Matches(aframe,
                "<a-entity line=\"start: (.*); end: (.*); color: (.*)\">"))
            {
                try
                {
                    GameObject line = new GameObject("Line", typeof(LineRenderer));
                    line.transform.SetParent(panel.transform, false);
                    LineRenderer lr = line.GetComponent<LineRenderer>();
                    string[] p1 = m.Groups[1].Value.Split(' ');
                    string[] p2 = m.Groups[2].Value.Split(' ');

                    Vector3 start = new Vector3(
                        float.Parse(p1[0], CultureInfo.InvariantCulture),
                        float.Parse(p1[1], CultureInfo.InvariantCulture),
                        float.Parse(p1[2], CultureInfo.InvariantCulture));

                    Vector3 end = new Vector3(
                        float.Parse(p2[0], CultureInfo.InvariantCulture),
                        float.Parse(p2[1], CultureInfo.InvariantCulture),
                        float.Parse(p2[2], CultureInfo.InvariantCulture));

                    lr.SetPosition(0, start);
                    lr.SetPosition(1, end);
                    Color c;
                    ColorUtility.TryParseHtmlString(m.Groups[3].Value, out c);
                    lr.startColor = c; lr.endColor = c;
                }
                catch
                {

                }
            }**/

            panel.transform.localPosition = new Vector3(-0.5f, -1f, 0f);

            EventTrigger trigger = canvas.GetComponent<EventTrigger>();
            InitializeBehaviour ib = canvas.GetComponent<InitializeBehaviour>();

            EventTrigger.Entry entry = new EventTrigger.Entry();
            entry.eventID = EventTriggerType.PointerDown;
            entry.callback.AddListener((data) => { ib.OnDrag(data); });
            trigger.triggers.Add(entry);

            EventTrigger.Entry entryTwo = new EventTrigger.Entry();
            entryTwo.eventID = EventTriggerType.PointerUp;
            entryTwo.callback.AddListener((data) => { ib.OnEndDrag(data); });
            trigger.triggers.Add(entryTwo);

            canvas.GetComponent<Canvas>().additionalShaderChannels =
                AdditionalCanvasShaderChannels.TexCoord1 |
                AdditionalCanvasShaderChannels.Normal |
                AdditionalCanvasShaderChannels.Tangent;

            return canvas;
        }
    }
}
