using System;
using System.Globalization;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using TMPro;
using UnityEngine.UI;

public class AFrameImporter : MonoBehaviour
{
    public GameObject AFrameCube;
    public GameObject AFrameSphere;
    public GameObject AFrameCylinder;
    public GameObject AFramePlane;
    public GameObject AFrameQuad;
    public GameObject AFrameCapsule;

    public void GenerateAFrame(string aframe)
    {
        aframe = aframe.Split(new string[] { "</html>" }, StringSplitOptions.None)[0];
        
        MatchCollection texts = Regex.Matches(aframe, "<a-entity (.*)text=\"(.*)\"(.*)>");
        MatchCollection geometries = Regex.Matches(aframe, "<a-entity (.*)geometry=\"(.*)\"(.*)>");
        MatchCollection lines = Regex.Matches(aframe, "<a-entity (.*)line=\"(.*)\"(.*)>");
        MatchCollection boxes = Regex.Matches(aframe, "<a-box (.*)>");
        MatchCollection spheres = Regex.Matches(aframe, "<a-sphere (.*)>");
        MatchCollection cylinders = Regex.Matches(aframe, "<a-cylinder (.*)>");
        MatchCollection planes = Regex.Matches(aframe, "<a-plane (.*)>");

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

        // ############################ TEXTS ########################################
        GenerateTexts(texts, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY);

        // ############################ GEOMETRIES ########################################
        GenerateGeometries(geometries, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping);

        // ############################# BOXES #########################################
        GenerateBoxes(boxes, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping);

        // ########################## SPHERES #########################################
        GenerateSpheres(spheres, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping);

        // ############################## CYLINDERS ######################################
        GenerateCylinders(cylinders, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping);

        // ########################## PLANES #############################################
        GeneratePlanes(planes, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping);

        // ########################### LINES #####################################
        GenerateLines(lines, aFramePanel.transform, pivot, ref maxX, ref maxY, ref minX, ref minY, geoMapping, lineMapping);

        float finalWidth = Math.Abs(maxX - minX);
        float finalHeight = Math.Abs(maxY - minY);

        aFrameCanvas.GetComponent<RectTransform>().sizeDelta = new Vector2(finalWidth, finalHeight);
        aFramePanel.GetComponent<RectTransform>().sizeDelta = new Vector2(finalWidth, finalHeight);

        float finalScale = (GetComponent<RectTransform>().sizeDelta.y / finalHeight) * 0.001f;
        float finalToolbarScale = 0.001f / finalScale;
        aFrameCanvas.transform.localScale = new Vector3(finalScale, finalScale, finalScale);
        aFrameCanvas.transform.Find("Panel/Toolbar").localScale =
            new Vector3(finalToolbarScale, finalToolbarScale, finalToolbarScale);

        /**
        float scaledWidth = finalWidth * finalScale;
        float scaledHeight = finalHeight * finalScale;

        Vector3 aFramePos = transform.TransformPoint(
            new Vector3(
                -.5f * (GetComponent<RectTransform>().sizeDelta.x + scaledWidth),
                transform.position.y < .5f * scaledHeight ?
                    .5f * scaledHeight - transform.position.y :
                    0f,
                0f));
        **/

        // Positioning
        aFrameCanvas.AddComponent<BoxCollider>();
        aFrameCanvas.GetComponent<InitializeBehaviour>().Initialize();
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="texts"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    private void GenerateTexts(MatchCollection texts, Transform parent, Vector3 pivot, 
        ref float maxX, ref float maxY, ref float minX, ref float minY)
    {
        // ############################ TEXTS ########################################

        foreach (Match m in texts)
        {
            string tag = "", value = "";
            Match posMatch = null, rotMatch = null, widthMatch = null, colorMatch = null;

            tag = m.Value;
            value = Regex.Match(tag, @"value:\s*([a-zA-Z0-9-.\s]+)\s*(;|"")").Groups[1].Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""\s*([0-9-.\se]+)\s*""");
            rotMatch = Regex.Match(tag, @"rotation\s*=\s*""\s*([0-9-.\se]+)\s*""");
            widthMatch = widthMatch = Regex.Match(tag, @"width:\s*([0-9-.e]+)\s*(;|"")");
            colorMatch = colorMatch = Regex.Match(tag, @"color:\s*([#0-9A-Z]+)\s*(;|"")");

            GameObject text = new GameObject(value, typeof(TextMeshProUGUI), typeof(ContentSizeFitter));
            text.tag = "AFrame";
            text.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                     Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                     i => float.Parse(i, CultureInfo.InvariantCulture));

                text.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            if (rotMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(rotMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                text.transform.localRotation =
                    Quaternion.Euler(coords[0], coords[1], coords[2]);
            }

            ContentSizeFitter fitter = text.GetComponent<ContentSizeFitter>();
            fitter.verticalFit = ContentSizeFitter.FitMode.PreferredSize;

            if (widthMatch.Success)
            {
                var sd = text.GetComponent<RectTransform>().sizeDelta;

                sd.x = float.Parse(
                    Regex.Replace(widthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

                text.GetComponent<RectTransform>().sizeDelta = sd;
            }

            if (colorMatch.Success)
            {
                TextMeshProUGUI textComponent = text.GetComponent<TextMeshProUGUI>();
                textComponent.text = "<size=0.1><color=" + colorMatch.Groups[1].Value + ">" + value + "</color></size>";
            }

            // Update width and height
            Vector2 size = text.GetComponent<RectTransform>().sizeDelta;

            maxX = Math.Max(maxX, text.transform.localPosition.x + size.x);
            maxY = Math.Max(maxY, text.transform.localPosition.y + size.y);
            minX = Math.Min(minX, text.transform.localPosition.x - size.x);
            minY = Math.Min(minY, text.transform.localPosition.y - size.y);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="geometries"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    /// <param name="geoMapping"></param>
    private void GenerateGeometries(MatchCollection geometries, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, Dictionary<Vector3, GameObject> geoMapping)
    {
        foreach (Match m in geometries)
        {
            string tag = m.Value;
            if (tag.Contains("src: #floor;"))
                continue;

            string primitive = "";
            Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null, radiusMatch = null,
                depthMatch = null, colorMatch = null, metalMatch = null, glossMatch = null, hoverColorMatch = null,
                modelMatch = null;

            primitive = Regex.Match(tag, @"primitive:\s*([a-zA-Z]+)\s*(;|"")").Groups[1].Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""\s*([0-9-.\se]+)\s*""");
            rotMatch = Regex.Match(tag, @"rotation\s*=\s*""\s*([0-9-.\se]+)\s*""");
            widthMatch = Regex.Match(tag, @"width:\s*([0-9-.e]+)\s*(;|"")");
            heightMatch = Regex.Match(tag, @"height:\s*([0-9-.e]+)\s*(;|"")");
            depthMatch = Regex.Match(tag, @"depth:\s*([0-9-.e]+)\s*(;|"")");
            colorMatch = Regex.Match(tag, @"color:\s*([#0-9A-Z]+)\s*(;|"")");
            metalMatch = Regex.Match(tag, @"metalness:\s*([0-9-.e]+)\s*(;|"")");
            glossMatch = Regex.Match(tag, @"roughness:\s*([0-9-.e]+)\s*(;|"")");
            hoverColorMatch = Regex.Match(tag, @"change-color-on-hover\s*=\s*""\s*color:\s*([#0-9A-Z]+)\s*""");
            radiusMatch = Regex.Match(tag, @"radius:\s*([0-9-.e]+)\s*(;|"")");
            modelMatch = Regex.Match(tag, @"model\s*=\s*""\s*([a-zA-Z0-9]+)\s*""");

            GameObject ob;
            if (primitive.Equals("box")) ob = Instantiate(AFrameCube);
            else if (primitive.Equals("cylinder")) ob = Instantiate(AFrameCylinder);
            else if (primitive.Equals("capsule")) ob = Instantiate(AFrameCapsule);
            else if (primitive.Equals("plane")) ob = Instantiate(AFramePlane);
            else if (primitive.Equals("sphere")) ob = Instantiate(AFrameSphere);
            else ob = Instantiate(AFrameQuad);

            ob.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            if (rotMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(rotMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localRotation =
                    Quaternion.Euler(coords[0], coords[1], coords[2]);
            }

            Vector3 scale = new Vector3(1f, 1f, 1f);
            if (widthMatch.Success && depthMatch.Success)
            {
                scale.x = float.Parse(
                    Regex.Replace(widthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

                scale.z = float.Parse(
                    Regex.Replace(depthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);
            }
            if (radiusMatch.Success)
            {
                scale.x = float.Parse(
                    Regex.Replace(radiusMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

                scale.z = float.Parse(
                    Regex.Replace(radiusMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);
            }
            if (heightMatch.Success)
                scale.y = float.Parse(
                    Regex.Replace(heightMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            ob.transform.localScale = scale;

            UpdateGeometryMaterial(ob, colorMatch, metalMatch, glossMatch);

            if (hoverColorMatch.Success)
            {
                Color c;
                ColorUtility.TryParseHtmlString(
                    hoverColorMatch.Groups[1].Value,
                    out c);

                ob.GetComponent<AFrameGeometry>().hoverColor = c;
            }

            if (modelMatch.Success)
            {
                TextMeshPro t = Instantiator.Instance.Text(modelMatch.Groups[1].Value, ob.transform);
                ob.GetComponent<AFrameGeometry>().t = t;
                t.gameObject.SetActive(false);
                t.transform.localPosition = -ob.transform.forward;
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
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="boxes"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    /// <param name="geoMapping"></param>
    private void GenerateBoxes(MatchCollection boxes, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, Dictionary<Vector3, GameObject> geoMapping)
    {
        foreach (Match m in boxes)
        {
            string tag = "";
            Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null, hoverColorMatch = null,
                depthMatch = null, colorMatch = null, metalMatch = null, glossMatch = null, modelMatch = null;

            tag = m.Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""\s*([0-9-.\se]+)\s*""");
            rotMatch = Regex.Match(tag, @"rotation\s*=\s*""\s*([0-9-.\se]+)\s*""");
            colorMatch = Regex.Match(tag, @"color\s*=\s*""\s*([#0-9A-Z]+)\s*""");
            widthMatch = Regex.Match(tag, @"width\s*=\s*""\s*([0-9-.e]+)\s*""");
            heightMatch = Regex.Match(tag, @"height\s*=\s*""\s*([0-9-.e]+)\s*""");
            depthMatch = Regex.Match(tag, @"depth\s*=\s*""\s*([0-9-.e]+)\s*""");
            metalMatch = Regex.Match(tag, @"metalness\s*=\s*""\s*([0-9-.e]+)\s*""");
            glossMatch = Regex.Match(tag, @"roughness\s*=\s*""\s*([0-9-.e]+)\s*""");
            hoverColorMatch = Regex.Match(tag, @"change-color-on-hover\s*=\s*""\s*color:\s*([#0-9A-Z]+)\s*""");
            modelMatch = Regex.Match(tag, @"model\s*=\s*""\s*([a-zA-Z0-9]+)\s*""");

            GameObject ob = Instantiate(AFrameCube);
            ob.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            if (rotMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(rotMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localRotation =
                    Quaternion.Euler(coords[0], coords[1], coords[2]);
            }

            Vector3 scale = new Vector3(1f, 1f, 1f);
            if (widthMatch.Success)
                scale.x = float.Parse(
                    Regex.Replace(widthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            if (heightMatch.Success)
                scale.y = float.Parse(
                    Regex.Replace(heightMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            if (depthMatch.Success)
                scale.z = float.Parse(
                    Regex.Replace(depthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            ob.transform.localScale = scale;

            UpdateGeometryMaterial(ob, colorMatch, metalMatch, glossMatch);

            if (hoverColorMatch.Success)
            {
                Color c;
                ColorUtility.TryParseHtmlString(
                    hoverColorMatch.Groups[1].Value,
                    out c);

                ob.GetComponent<AFrameGeometry>().hoverColor = c;
            }

            if (modelMatch.Success)
            {
                TextMeshPro t = Instantiator.Instance.Text(modelMatch.Groups[1].Value, ob.transform);
                ob.GetComponent<AFrameGeometry>().t = t;
                t.gameObject.SetActive(false);
                t.transform.localPosition = -ob.transform.forward;
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
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="spheres"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    /// <param name="geoMapping"></param>
    private void GenerateSpheres(MatchCollection spheres, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, Dictionary<Vector3, GameObject> geoMapping)
    {
        foreach (Match m in spheres)
        {
            string tag = "";
            Match posMatch = null, colorMatch = null, metalMatch = null, modelMatch = null,
                glossMatch = null, radiusMatch = null, hoverColorMatch = null;

            tag = m.Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""\s*([0-9-.\se]+)\s*""");
            colorMatch = Regex.Match(tag, @"color\s*=\s*""\s*([#0-9A-Z]+)\s*""");
            metalMatch = Regex.Match(tag, @"metalness\s*=\s*""\s*([0-9-.e]+)\s*""");
            glossMatch = Regex.Match(tag, @"roughness\s*=\s*""\s*([0-9-.e]+)\s*""");
            radiusMatch = Regex.Match(tag, @"radius\s*=\s*""\s*([0-9-.\se]+)\s*""");
            hoverColorMatch = Regex.Match(tag, @"change-color-on-hover\s*=\s*""\s*color:\s*([#0-9A-Z]+)\s*""");
            modelMatch = Regex.Match(tag, @"model\s*=\s*""\s*([a-zA-Z0-9]+)\s*""");

            GameObject ob = Instantiate(AFrameSphere);
            ob.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            Vector3 scale = new Vector3(1f, 1f, 1f);
            if (radiusMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(radiusMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localScale = new Vector3(coords[0], coords[1], coords[2]);
            }

            UpdateGeometryMaterial(ob, colorMatch, metalMatch, glossMatch);

            if (hoverColorMatch.Success)
            {
                Color c;
                ColorUtility.TryParseHtmlString(
                    hoverColorMatch.Groups[1].Value,
                    out c);

                ob.GetComponent<AFrameGeometry>().hoverColor = c;
            }

            if (modelMatch.Success)
            {
                TextMeshPro t = Instantiator.Instance.Text(modelMatch.Groups[1].Value, ob.transform);
                ob.GetComponent<AFrameGeometry>().t = t;
                t.gameObject.SetActive(false);
                t.transform.localPosition = -ob.transform.forward;
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
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="cylinders"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    /// <param name="geoMapping"></param>
    private void GenerateCylinders(MatchCollection cylinders, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, Dictionary<Vector3, GameObject> geoMapping)
    {
        foreach (Match m in cylinders)
        {
            string tag = "";
            Match posMatch = null, colorMatch = null, metalMatch = null, glossMatch = null, modelMatch = null,
                radiusMatch = null, heightMatch = null, rotMatch = null, hoverColorMatch = null;

            tag = m.Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""([0-9-.\se]+)\s*""");
            colorMatch = Regex.Match(tag, @"color\s*=\s*""\s*([#0-9A-Z]+)\s*""");
            metalMatch = Regex.Match(tag, @"metalness\s*=\s*""\s*([0-9-.e]+)\s*""");
            glossMatch = Regex.Match(tag, @"roughness\s*=\s*""\s*([0-9-.e]+)\s*""");
            radiusMatch = Regex.Match(tag, @"radius\s*=\s*""\s*([0-9-.e]+)\s*""");
            heightMatch = Regex.Match(tag, @"height\s*=\s*""\s*([0-9-.e]+)\s*""");
            rotMatch = Regex.Match(tag, @"rotation\s*=\s*""\s*([0-9-.\se]+)\s*""");
            hoverColorMatch = Regex.Match(tag, @"change-color-on-hover\s*=\s*""\s*color:\s*([#0-9A-Z]+)\s*""");
            modelMatch = Regex.Match(tag, @"model\s*=\s*""\s*([a-zA-Z0-9]+)\s*""");

            GameObject ob = Instantiate(AFrameCylinder);
            ob.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            if (rotMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(rotMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localRotation =
                    Quaternion.Euler(coords[0], coords[1], coords[2]);
            }

            Vector3 scale = new Vector3(1f, 1f, 1f);
            if (radiusMatch.Success)
            {
                float radius = float.Parse(
                    Regex.Replace(radiusMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

                scale.x = radius;
                scale.z = radius;
            }

            if (heightMatch.Success)
            {
                float h = float.Parse(
                    Regex.Replace(heightMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

                scale.y = h;
            }

            ob.transform.localScale = scale;

            UpdateGeometryMaterial(ob, colorMatch, metalMatch, glossMatch);

            if (modelMatch.Success)
            {
                TextMeshPro t = Instantiator.Instance.Text(modelMatch.Groups[1].Value, ob.transform);
                ob.GetComponent<AFrameGeometry>().t = t;
                t.gameObject.SetActive(false);
                t.transform.localPosition = -ob.transform.forward;
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
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="planes"></param>
    /// <param name="parent"></param>
    /// <param name="pivot"></param>
    /// <param name="maxX"></param>
    /// <param name="maxY"></param>
    /// <param name="minX"></param>
    /// <param name="minY"></param>
    /// <param name="geoMapping"></param>
    private void GeneratePlanes(MatchCollection planes, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, Dictionary<Vector3, GameObject> geoMapping)
    {
        foreach (Match m in planes)
        {
            string tag = "";
            Match posMatch = null, rotMatch = null, widthMatch = null, heightMatch = null, modelMatch = null,
                colorMatch = null, metalMatch = null, glossMatch = null, hoverColorMatch = null, depthMatch = null;

            tag = m.Value;
            posMatch = Regex.Match(tag, @"position\s*=\s*""\s*([0-9-.\se]+)\s*""");
            rotMatch = Regex.Match(tag, @"rotation\s*=\s*""\s*([0-9-.\se]+)\s*""");
            colorMatch = Regex.Match(tag, @"color\s*=\s*""\s*([#0-9A-Z]+)\s*""");
            widthMatch = Regex.Match(tag, @"width\s*=\s*""\s*([0-9-.e]+)\s*""");
            depthMatch = Regex.Match(tag, @"depth\s*=\s*""\s*([0-9-.e]+)\s*""");
            heightMatch = Regex.Match(tag, @"height\s*=\s*""\s*([0-9-.e]+)\s*""");
            metalMatch = Regex.Match(tag, @"metalness\s*=\s*""\s*([0-9-.e]+)\s*""");
            glossMatch = Regex.Match(tag, @"roughness\s*=\s*""\s*([0-9-.e]+)\s*""");
            hoverColorMatch = Regex.Match(tag, @"change-color-on-hover\s*=\s*""\s*color:\s*([#0-9A-Z]+)\s*""");
            modelMatch = Regex.Match(tag, @"model\s*=\s*""\s*([a-zA-Z0-9]+)\s*""");

            GameObject ob = Instantiate(AFramePlane);
            ob.transform.SetParent(parent, false);

            if (posMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                     Regex.Replace(posMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                     i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localPosition =
                    new Vector3(coords[0], coords[1], coords[2]) - pivot;
            }

            if (rotMatch.Success)
            {
                float[] coords = Array.ConvertAll(
                    Regex.Replace(rotMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                ob.transform.localRotation =
                    Quaternion.Euler(coords[0], coords[1], coords[2]);
            }

            Vector3 scale = new Vector3(1f, 1f, 1f);
            if (widthMatch.Success)
                scale.x = float.Parse(
                    Regex.Replace(widthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            if (heightMatch.Success)
                scale.y = float.Parse(
                    Regex.Replace(heightMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            if (depthMatch.Success)
                scale.z = float.Parse(
                    Regex.Replace(depthMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"),
                    CultureInfo.InvariantCulture);

            ob.transform.localScale = scale;

            UpdateGeometryMaterial(ob, colorMatch, metalMatch, glossMatch);

            if (hoverColorMatch.Success)
            {
                Color c;
                ColorUtility.TryParseHtmlString(hoverColorMatch.Groups[1].Value, out c);
                ob.GetComponent<AFrameGeometry>().hoverColor = c;
            }

            if (modelMatch.Success)
            {
                TextMeshPro t = Instantiator.Instance.Text(modelMatch.Groups[1].Value, ob.transform);
                ob.GetComponent<AFrameGeometry>().t = t;
                t.gameObject.SetActive(false);
                t.transform.localPosition = -ob.transform.forward;
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
    }

    private void GenerateLines(MatchCollection lines, Transform parent, Vector3 pivot,
        ref float maxX, ref float maxY, ref float minX, ref float minY, 
        Dictionary<Vector3, GameObject> geoMapping, Dictionary<Vector3, GameObject> lineMapping)
    {
        foreach (Match m in lines)
        {
            string tag = "";
            Match startMatch = null, endMatch = null, colorMatch = null;

            tag = m.Value;
            startMatch = Regex.Match(tag, @"start: ([0-9-.\se]+)(;|"")");
            endMatch = Regex.Match(tag, @"end: ([0-9-.\se]+)(;|"")");
            colorMatch = Regex.Match(tag, @"color: ([#0-9A-Z]+)(;|"")");

            GameObject line = GameObject.CreatePrimitive(PrimitiveType.Cube);
            line.name = "Line";
            line.tag = "AFrame";
            line.AddComponent<AFrameLine>();
            line.transform.SetParent(parent, false);

            if (startMatch.Success && endMatch.Success)
            {
                float[] s = Array.ConvertAll(
                    Regex.Replace(startMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
                    i => float.Parse(i, CultureInfo.InvariantCulture));

                float[] e = Array.ConvertAll(
                    Regex.Replace(endMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
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
                        GameObject ob = Instantiate(AFrameSphere);
                        ob.transform.SetParent(parent, false);
                        ob.transform.localScale = new Vector3(.06f, .06f, .06f);
                        ob.transform.localPosition = afl.start;
                        geoMapping.Add(start, ob);

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
                        GameObject ob = Instantiate(AFrameSphere);
                        ob.transform.SetParent(parent, false);
                        ob.transform.localScale = new Vector3(.06f, .06f, .06f);
                        ob.transform.localPosition = afl.end;
                        geoMapping.Add(end, ob);

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
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="aFrame"></param>
    /// <returns></returns>
    private Vector3 GetNearestObjectToOrigin(string aFrame)
    {
        float minMagnitude = float.MaxValue;
        Vector3 pivot = Vector3.zero;
        MatchCollection c = Regex.Matches(aFrame, @"(start|end|position)(:\s|=""\s?)([0-9-.\se]+)(;|"")");

        foreach (Match m in c)
        {
            float[] coords = Array.ConvertAll(
                    Regex.Replace(m.Groups[3].Value, @"([0-9-.]+)e-([0-9.]+)", "0").Split(' '),
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

    /// <summary>
    /// 
    /// </summary>
    /// <param name="ob"></param>
    /// <param name="colorMatch"></param>
    /// <param name="metalMatch"></param>
    /// <param name="glossMatch"></param>
    private void UpdateGeometryMaterial(GameObject ob, Match colorMatch, Match metalMatch, Match glossMatch)
    {
        if (colorMatch.Success || glossMatch.Success || metalMatch.Success)
        {
            Material material = ob.GetComponent<Renderer>().material;

            if (colorMatch.Success)
            {
                Color c;
                ColorUtility.TryParseHtmlString(colorMatch.Groups[1].Value, out c);
                material.color = c;
                ob.GetComponent<AFrameGeometry>().baseColor = c;
            }

            if (glossMatch.Success)
            {
                float value = float.Parse(Regex.Replace(glossMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"), CultureInfo.InvariantCulture);
                material.SetFloat("_Glossiness", 1f - value);
            }

            if (metalMatch.Success)
            {
                float value = float.Parse(Regex.Replace(metalMatch.Groups[1].Value, @"([0-9-.]+)e-([0-9.]+)", "0"), CultureInfo.InvariantCulture);
                material.SetFloat("_Metallic", value);
            }
        }
    }
}