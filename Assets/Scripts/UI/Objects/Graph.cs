using ImageUtils;
using LoggingModule;
using SaveAndLoad;
using Unity.VectorGraphics;

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
        if (loadingWheel == null || !loadingWheel.activeSelf)
        {
            SaveAndLoadModule.graphs.Remove(this);
            InteractionLogger.Discount("GraphObject");
            Destroy(gameObject);
        }
    }
}
