﻿using UnityEngine;
using UnityEngine.UI;
using ImageUtils;
using LoggingModule;
using SaveAndLoad;

public class Graph : InitializeBehaviour
{
    public SVGImage graph_panel;
    public string raw_image;
    public string type;

    void Start() { }

    public void setSprite(string raw, string tp)
    {
        raw_image = raw;
        type = tp;
        graph_panel.sprite = type == "SVG" ?
            ImageModule.ImportSVG(raw) :
            ImageModule.ImportPNG(raw);

        float width = graph_panel.sprite.texture.width;
        float height = graph_panel.sprite.texture.height;

        var sd = GetComponent<RectTransform>().sizeDelta;
        sd.x = width * sd.y / height;
        GetComponent<RectTransform>().sizeDelta = sd;
    }

    public override void onClose()
    {
        SaveAndLoadModule.graphs.Remove(this);
        InteractionLogger.Discount("GraphObject");
        Destroy(gameObject);
    }
}
