using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor;
using UnityEngine.TestTools;
using System.Collections;
using System.Collections.Generic;
using Unity.VectorGraphics;

public class StyleResolverTests
{
    const string k_GlobalStyle = @"
            <![CDATA[
            * {
                fill: lime;
            }
            .blueish {
                fill: blue;
            }
            #myid {
                fill: magenta;
            }
            rect {
                fill: cyan;
            }
        ]]>
    ";

    [Test]
    public void StyleResolver_MatchesElementName()
    {
        string svg =
            string.Format(
            @"<svg xmlns=""http://www.w3.org/2000/svg"" width=""100"" height=""20"">
                <defs>
                    <style>{0}</style>
                </defs>
                <rect width=""100"" height=""20"" />
            </svg>", k_GlobalStyle);

        var sceneInfo = SVGParser.ImportSVG(new StringReader(svg));
        var node = sceneInfo.Scene.Root.Children[0];
        var solidFill = node.Shapes[0].Fill as SolidFill;
        Assert.AreEqual(Color.cyan, solidFill.Color);
    }

    [Test]
    public void StyleResolver_MatchesClassOverElementName()
    {
        string svg =
            string.Format(
            @"<svg xmlns=""http://www.w3.org/2000/svg"" width=""100"" height=""20"">
                <defs>
                    <style>{0}</style>
                </defs>
                <rect class=""blueish"" width=""100"" height=""20"" />
            </svg>", k_GlobalStyle);

        var sceneInfo = SVGParser.ImportSVG(new StringReader(svg));
        var node = sceneInfo.Scene.Root.Children[0];
        var solidFill = node.Shapes[0].Fill as SolidFill;
        Assert.AreEqual(Color.blue, solidFill.Color);
    }

    [Test]
    public void StyleResolver_MatchesIDOverClassOrElementName()
    {
        string svg =
            string.Format(
            @"<svg xmlns=""http://www.w3.org/2000/svg"" width=""100"" height=""20"">
                <defs>
                    <style>{0}</style>
                </defs>
                <rect id=""myid"" class=""blueish"" width=""100"" height=""20"" />
            </svg>", k_GlobalStyle);

        var sceneInfo = SVGParser.ImportSVG(new StringReader(svg));
        var node = sceneInfo.Scene.Root.Children[0];
        var solidFill = node.Shapes[0].Fill as SolidFill;
        Assert.AreEqual(Color.magenta, solidFill.Color);
    }

    [Test]
    public void StyleResolver_MatchesStar()
    {
        string svg =
            string.Format(
            @"<svg xmlns=""http://www.w3.org/2000/svg"" width=""100"" height=""20"">
                <defs>
                    <style>{0}</style>
                </defs>
                <circle cx=""50"" cy=""50"" r=""50"" />
            </svg>", k_GlobalStyle);

        var sceneInfo = SVGParser.ImportSVG(new StringReader(svg));
        var node = sceneInfo.Scene.Root.Children[0];
        var solidFill = node.Shapes[0].Fill as SolidFill;
        Assert.AreEqual(Color.green, solidFill.Color);
    }

    [Test]
    public void StyleResolver_MatchesAttributeIfNoCSSStyleMatches()
    {
        string svg =
            string.Format(
            @"<svg xmlns=""http://www.w3.org/2000/svg"" width=""100"" height=""20"">
                <defs>
                    <style>{0}</style>
                </defs>
                <circle cx=""50"" cy=""50"" r=""50"" stroke=""red"" />
            </svg>", k_GlobalStyle);

        var sceneInfo = SVGParser.ImportSVG(new StringReader(svg));
        var node = sceneInfo.Scene.Root.Children[0];
        var stroke = node.Shapes[0].PathProps.Stroke;
        Assert.IsNotNull(stroke);
        Assert.AreEqual(Color.red, stroke.Color);
    }
}
