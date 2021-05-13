using ImageUtils;
using LoggingModule;
using SaveAndLoad;
using Unity.VectorGraphics;
using UnityEngine;

public class Graph : InitializeBehaviour
{
    public SVGImage graph_panel;

    [HideInInspector]
    public string raw_image;
    [HideInInspector]
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
        if (!SomethingIsLoading())
        {
            SaveAndLoadModule.graphs.Remove(this);
            InteractionLogger.Discount("GraphObject", GetInstanceID().ToString());
            Destroy(gameObject);
        }
    }
}
