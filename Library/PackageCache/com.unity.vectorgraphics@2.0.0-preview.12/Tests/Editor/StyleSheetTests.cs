using NUnit.Framework;
using UnityEngine;
using UnityEditor;
using UnityEngine.TestTools;
using System.Collections;
using System.Collections.Generic;
using Unity.VectorGraphics;

public class StyleSheetTests
{
    [Test]
    public void Tokenize_TokenizesTokens()
    {
        var cssText = "sel { name:value; }";
        var tokens = SVGStyleSheetUtils.Tokenize(cssText);
        Assert.AreEqual(7, tokens.Count);
        Assert.AreEqual("sel",   tokens[0]);
        Assert.AreEqual("{",     tokens[1]);
        Assert.AreEqual("name",  tokens[2]);
        Assert.AreEqual(":",     tokens[3]);
        Assert.AreEqual("value", tokens[4]);
        Assert.AreEqual(";",     tokens[5]);
        Assert.AreEqual("}",     tokens[6]);        
    } 

    [Test]
    public void Tokenize_IgnoresComments()
    {
        var cssText = "{ /*hey*/ value /*there*/ }";
        var tokens = SVGStyleSheetUtils.Tokenize(cssText);
        Assert.AreEqual(3, tokens.Count);
        Assert.AreEqual("{",     tokens[0]);
        Assert.AreEqual("value", tokens[1]);
        Assert.AreEqual("}",     tokens[2]);        
    } 

    [Test]
    public void Parse_WithSingleSelector_ParsesOneSelector()
    {
        var cssText = "sel { name1:value1; name2:value2; }";
        var sheet = SVGStyleSheetUtils.Parse(cssText);
        Assert.AreEqual(1, sheet.Count);

        var props = sheet["sel"];
        Assert.AreEqual(2, props.Count);
        Assert.AreEqual("value1", props["name1"]);
        Assert.AreEqual("value2", props["name2"]);
    }

    [Test]
    public void Parse_WithTwoSelectors_ParsesTwoSelectors()
    {
        var cssText = "sel1 { name1:value1; } sel2 { name2:value2 }";
        var sheet = SVGStyleSheetUtils.Parse(cssText);
        Assert.AreEqual(2, sheet.Count);

        var props = sheet["sel1"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("value1", props["name1"]);

        props = sheet["sel2"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("value2", props["name2"]);
    }

    [Test]
    public void Parse_WithMultiSelectors_GeneratesTwoSelectors()
    {
        var cssText = "sel1,sel2 { name1:value1 }";
        var sheet = SVGStyleSheetUtils.Parse(cssText);
        Assert.AreEqual(2, sheet.Count);

        var props = sheet["sel1"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("value1", props["name1"]);

        props = sheet["sel2"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("value1", props["name1"]);
    }

    [Test]
    public void ParseInline_ParsesInlineProperties()
    {
        var cssText = "name1:value1; name2:value2";
        var properties = SVGStyleSheetUtils.ParseInline(cssText);
        Assert.AreEqual(2, properties.Count);
        Assert.AreEqual("value1", properties["name1"]);
        Assert.AreEqual("value2", properties["name2"]);
    }

    [Test]
    public void Parse_WithSharedSelector_CombinesProperties()
    {
        var cssText = ".cls1,.cls2 { fill:none } .cls1 { stroke:red }";
        var sheet = SVGStyleSheetUtils.Parse(cssText);
        Assert.AreEqual(2, sheet.Count);

        var props = sheet[".cls1"];
        Assert.AreEqual(2, props.Count);
        Assert.AreEqual("none", props["fill"]);
        Assert.AreEqual("red", props["stroke"]);

        props = sheet[".cls2"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("none", props["fill"]);
    }

    [Test]
    public void Parse_SupportMultiValues()
    {
        var cssText = "rect { stroke-dasharray: 10 10; }";
        var sheet = SVGStyleSheetUtils.Parse(cssText);
        Assert.AreEqual(1, sheet.Count);

        var props = sheet["rect"];
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("10 10", props["stroke-dasharray"]);
    }

    [Test]
    public void ParseInline_SupportMultiValues()
    {
        var cssText = "stroke-dasharray: 10 10";
        var props = SVGStyleSheetUtils.ParseInline(cssText);
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual("10 10", props["stroke-dasharray"]);
    }

    [Test]
    public void ParseInline_SupportsData()
    {
        var cssText = @"font-family: url(""data:font/woff2;base64,d09GMgABAAAAACx8AA"")";
        var props = SVGStyleSheetUtils.ParseInline(cssText);
        Assert.AreEqual(1, props.Count);
        Assert.AreEqual(@"url(""data:font/woff2;base64,d09GMgABAAAAACx8AA"")", props["font-family"]);
    }
}
