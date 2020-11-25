using System;
using System.Linq;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.U2D;
using UnityEngine.Experimental.U2D;
using UnityEngine.Experimental.Rendering;
using Unity.VectorGraphics;
using Unity.VectorGraphics.Editor;

public class CornerTests : MonoBehaviour
{
    void Start()
    {
        float x = -1.0f;
        float xStep = 0.48f;

        var options = new VectorUtils.TessellationOptions();
        options.MaxCordDeviation = float.MaxValue;
        options.MaxTanAngleDeviation = Mathf.PI * 0.5f;
        options.SamplingStepSize = 1.0f / 100.0f;

        // Fine tessellation
        options.StepDistance = 1.0f;
        BuildTestColumnWithProperties(x, 1.0f, PathEnding.Chop, PathCorner.Tipped, options);  x += xStep;
        BuildTestColumnWithProperties(x, 2.0f, PathEnding.Chop, PathCorner.Tipped, options);  x += xStep;
        BuildTestColumnWithProperties(x, 4.0f, PathEnding.Chop, PathCorner.Beveled, options); x += xStep;
        BuildTestColumnWithProperties(x, 6.0f, PathEnding.Round, PathCorner.Round, options);  x += xStep;

        // Coarse tessellation
        options.StepDistance = 100.0f;
        BuildTestColumnWithProperties(x, 1.0f, PathEnding.Chop, PathCorner.Tipped, options);  x += xStep;
        BuildTestColumnWithProperties(x, 2.0f, PathEnding.Chop, PathCorner.Tipped, options);  x += xStep;
        BuildTestColumnWithProperties(x, 4.0f, PathEnding.Chop, PathCorner.Beveled, options); x += xStep;
        BuildTestColumnWithProperties(x, 6.0f, PathEnding.Round, PathCorner.Round, options);  x += xStep;
    }

    private static void BuildTestColumnWithProperties(float x, float width, PathEnding ending, PathCorner corners, VectorUtils.TessellationOptions options)
    {
        var sprites = new List<Sprite>();

        var angles = new float[] {
            -Mathf.PI + Mathf.PI / 8,
            -Mathf.PI + Mathf.PI / 4,
            Mathf.PI - Mathf.PI / 4,
            Mathf.PI - Mathf.PI / 8
        };

        foreach (var angle in angles)
        {
            var path = LinesWithAngle(angle, width);
            var pathProps = path.PathProps;
            pathProps.Head = ending;
            pathProps.Tail = ending;
            pathProps.Corners = corners;
            path.PathProps = pathProps;

            var geoms = new List<VectorUtils.Geometry>();
            TessellatePath(path.Contours[0], path.PathProps, geoms, options);
            sprites.Add(SpriteFromGeometry(geoms));
        }

        var pos = new Vector2(x, 0.0f);
        foreach (var sprite in sprites)
        {
            var go = new GameObject("Path");
            go.transform.position = pos;
            var sr = go.AddComponent<SpriteRenderer>();
            sr.sprite = sprite;
            pos.y += 0.25f;
        }
    }

    private static Shape LinesWithAngle(float angle, float width)
    {
        var p = Vector2.zero;
        var q = new Vector2(20.0f, 0.0f) + p;
        var r = new Vector2(Mathf.Cos(angle) * 20.0f, Mathf.Sin(angle) * 20.0f) + q;
        var path = new Shape() {
            Contours = new BezierContour[] {
                new BezierContour() {
                    Segments = new BezierPathSegment[] {
                        new BezierPathSegment() { P0 = p, P1 = p + (q - p) / 3.0f, P2 = p + (q - p) / 3.0f * 2.0f },
                        new BezierPathSegment() { P0 = q, P1 = q + (r - q) / 3.0f, P2 = q + (r - q) / 3.0f * 2.0f },
                        new BezierPathSegment() { P0 = r }
                    }
                }
            },
            PathProps = new PathProperties() {
                Stroke = new Stroke() { Color = Color.white, HalfThickness = width / 2 }
            }
        };
        return path;
    }

    private static Sprite SpriteFromGeometry(List<VectorUtils.Geometry> geoms)
    {
        var vertices = new List<Vector2>();
        var indices = new List<UInt16>();
        var colors = new List<Color>();

        foreach (var geom in geoms)
        {
            if (geom.Indices.Length == 0)
                continue;

            indices.AddRange(geom.Indices.Select(x => (UInt16)(x + vertices.Count)));
            vertices.AddRange(geom.Vertices.Select(x => geom.WorldTransform * x));
            colors.AddRange(Enumerable.Repeat(geom.Color, geom.Vertices.Length));
        }

        var bbox = VectorUtils.Bounds(vertices);
        VectorUtils.RealignVerticesInBounds(vertices, bbox, true);
        var rect = new Rect(0, 0, bbox.width, bbox.height);

        // The Sprite.Create(Rect, Vector2, float, Texture2D) method is internal. Using reflection
        // until it becomes public.
        var spriteCreateMethod = typeof(Sprite).GetMethod("Create", BindingFlags.Static | BindingFlags.NonPublic, Type.DefaultBinder, new Type[] { typeof(Rect), typeof(Vector2), typeof(float), typeof(Texture2D) }, null);
        var sprite = spriteCreateMethod.Invoke(null, new object[] { rect, Vector2.zero, 100.0f, null }) as Sprite;

        sprite.OverrideGeometry(vertices.ToArray(), indices.ToArray());

        var colors32 = colors.Select(c => (Color32)c);
        using (var nativeColors = new NativeArray<Color32>(colors32.ToArray(), Allocator.Temp))
            sprite.SetVertexAttribute<Color32>(VertexAttribute.Color, nativeColors);

        return sprite;
    }

    private static void TessellatePath(BezierContour contour, PathProperties pathProps, List<VectorUtils.Geometry> geoms, VectorUtils.TessellationOptions options)
    {
        if (pathProps.Stroke != null)
        {
            Vector2[] vertices;
            UInt16[] indices;
            VectorUtils.TessellatePath(contour, pathProps, options, out vertices, out indices);

            var color = pathProps.Stroke.Color;
            geoms.Add(new VectorUtils.Geometry() { Vertices = vertices, Indices = indices, Color = color });
        }
    }
}
