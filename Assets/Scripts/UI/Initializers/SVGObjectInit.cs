using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using Unity.VectorGraphics;
using TMPro;

public class SVGObjectInit : InitializeBehaviour
{
    public SVGImage graph_panel;

    public void setSprite(Sprite sp)
    {
        graph_panel.sprite = sp;
    }

}
