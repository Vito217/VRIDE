using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using Unity.VectorGraphics;
using ImageUtils;
using TMPro;
using LoggingModule;

public class Graph : InitializeBehaviour
{
    public SVGImage graph_panel;
    public string raw_image;
    public string type;

    public void setSprite(string raw, string tp)
    {
        raw_image = raw;
        type = tp;
        graph_panel.sprite = type == "SVG" ?
            ImageModule.ImportSVG(raw) :
            ImageModule.ImportPNG(raw);
    }

    public override void onClose()
    {
        player.graphs.Remove(this);
        InteractionLogger.Discount("GraphObject");
        Destroy(gameObject);
    }
}
