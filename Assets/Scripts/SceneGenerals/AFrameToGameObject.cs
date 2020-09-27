using System;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

namespace AFrameToGameObject
{
    public static class AFrameToGameObject
    {
        public static void Convert(String aframe)
        {
            // Base canvas
            GameObject canvas = new GameObject("AFrame", typeof(Canvas));
            canvas.GetComponent<Canvas>().renderMode = RenderMode.WorldSpace;

            foreach (Match m in Regex.Matches(aframe,
                "<a-entity position=\"(.*)\" text=\"width: (.*); color: (.*); value: (.*)\" >"))
            {
                // A text object
                GameObject text = new GameObject(m.Groups[4].Value, typeof(TextMeshProUGUI));
                text.transform.SetParent(canvas.transform);

                string[] p = m.Groups[1].Value.Split(' ');
                Vector3 position = new Vector3(float.Parse(p[0]), float.Parse(p[1]), float.Parse(p[2]));
                text.transform.localPosition = position;
                TextMeshProUGUI textComponent = text.GetComponent<TextMeshProUGUI>();
                textComponent.alignment = TextAlignmentOptions.Center | TextAlignmentOptions.Midline;
                textComponent.text = "<color=" + m.Groups[3].Value + ">" + m.Groups[4].Value + "</color>";
                textComponent.fontSize = 1;                
            }

            foreach (Match m in Regex.Matches(aframe,
                "<a-entity position=\"(.*)\" geometry=\"primitive: (.*); width: (.*); heigth: (.*); depth: (.*)\" material=\"color: (.*); roughness: (.*); metalness: (.*)\" >"))
            {
                GameObject ob;
                string primitive = m.Groups[2].Value;
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
                ob.transform.SetParent(canvas.transform);

                string[] p = m.Groups[1].Value.Split(' ');
                Vector3 position = new Vector3(float.Parse(p[0]), float.Parse(p[1]), float.Parse(p[2]));
                ob.transform.localPosition = position;
                ob.transform.localScale = new Vector3(
                    float.Parse(m.Groups[3].Value), 
                    float.Parse(m.Groups[4].Value), 
                    float.Parse(m.Groups[5].Value));
                Color c;
                ColorUtility.TryParseHtmlString(m.Groups[6].Value, out c);
                Material material = ob.GetComponent<Renderer>().material;
                material.color = c;
                material.SetFloat("_Glossiness", 1f - float.Parse(m.Groups[7].Value));
                material.SetFloat("_Metallic", float.Parse(m.Groups[8].Value));
            }
            foreach (Match m in Regex.Matches(aframe,
                "<a-entity line=\"start: (.*); end: (.*); color: (.*)\" >"))
            {
                GameObject line = new GameObject("Line", typeof(LineRenderer));
                LineRenderer lr = line.GetComponent<LineRenderer>();
                string[] p1 = m.Groups[1].Value.Split(' ');
                string[] p2 = m.Groups[2].Value.Split(' ');
                Vector3 start = new Vector3(float.Parse(p1[0]), float.Parse(p1[1]), float.Parse(p1[2]));
                Vector3 end = new Vector3(float.Parse(p2[0]), float.Parse(p2[1]), float.Parse(p2[2]));
                lr.SetPosition(0, start);
                lr.SetPosition(0, end);
                Color c;
                ColorUtility.TryParseHtmlString(m.Groups[3].Value, out c);
                lr.startColor = c; lr.endColor = c;
            }
        }
    }
}
