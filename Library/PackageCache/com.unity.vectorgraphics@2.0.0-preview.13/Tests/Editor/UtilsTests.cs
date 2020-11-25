using System;
using System.Linq;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using UnityEngine.TestTools;
using Unity.VectorGraphics;

public class UtilsTests
{
    [Test]
    public void BezierSegmentToPath_ReturnsBezierPathSegments()
    {
        var points = new Vector2[4];
        for (int i = 0; i < points.Length; ++i)
            points[i] = Vector2.one * i;

        var seg = new BezierSegment() { P0 = points[0], P1 = points[1], P2 = points[2], P3 = points[3] };
        var pathSeg = VectorUtils.BezierSegmentToPath(seg);

        Assert.AreEqual(2, pathSeg.Length);
        Assert.AreEqual(points[0], pathSeg[0].P0);
        Assert.AreEqual(points[1], pathSeg[0].P1);
        Assert.AreEqual(points[2], pathSeg[0].P2);
        Assert.AreEqual(points[3], pathSeg[1].P0);
    }

    [Test]
    public void BezierSegmentsToPath_WithConnectedSegments_ReturnsConnectedPath()
    {
        var segments = new BezierSegment[] {
            new BezierSegment() {
                P0 = new Vector2(0,0), P1 = new Vector2(1,1), P2 = new Vector2(2,2), P3 = new Vector2(3,3)
            },
            new BezierSegment() {
                P0 = new Vector2(3,3), P1 = new Vector2(4,4), P2 = new Vector2(5,5), P3 = new Vector2(6,6)
            }
        };

        var path = VectorUtils.BezierSegmentsToPath(segments);
        Assert.AreEqual(3, path.Length);
        
        var pathSeg = path[0];
        Assert.AreEqual(new Vector2(0, 0), pathSeg.P0);
        Assert.AreEqual(new Vector2(1, 1), pathSeg.P1);
        Assert.AreEqual(new Vector2(2, 2), pathSeg.P2);

        pathSeg = path[1];
        Assert.AreEqual(new Vector2(3, 3), pathSeg.P0);
        Assert.AreEqual(new Vector2(4, 4), pathSeg.P1);
        Assert.AreEqual(new Vector2(5, 5), pathSeg.P2);

        pathSeg = path[2];
        Assert.AreEqual(new Vector2(6, 6), pathSeg.P0);
    }

    [Test]
    public void BezierSegmentsToPath_WithDisconnectedSegments_ReturnsConnectedPathWithStraightLine()
    {
        var segments = new BezierSegment[] {
            new BezierSegment() {
                P0 = new Vector2(0,0), P1 = new Vector2(1,1), P2 = new Vector2(2,2), P3 = new Vector2(3,3)
            },
            new BezierSegment() {
                P0 = new Vector2(6,6), P1 = new Vector2(7,7), P2 = new Vector2(8,8), P3 = new Vector2(9,9)
            }
        };

        var path = VectorUtils.BezierSegmentsToPath(segments);
        Assert.AreEqual(4, path.Length);

        var pathSeg = path[0];
        Assert.AreEqual(new Vector2(0, 0), pathSeg.P0);
        Assert.AreEqual(new Vector2(1, 1), pathSeg.P1);
        Assert.AreEqual(new Vector2(2, 2), pathSeg.P2);

        // The second segment should be a straight line between (3,3) and (6,6)
        pathSeg = path[1];
        Assert.AreEqual(new Vector2(3, 3), pathSeg.P0);
        Assert.AreEqual(new Vector2(4, 4), pathSeg.P1);
        Assert.AreEqual(new Vector2(5, 5), pathSeg.P2);

        pathSeg = path[2];
        Assert.AreEqual(new Vector2(6, 6), pathSeg.P0);
        Assert.AreEqual(new Vector2(7, 7), pathSeg.P1);
        Assert.AreEqual(new Vector2(8, 8), pathSeg.P2);

        pathSeg = path[3];
        Assert.AreEqual(new Vector2(9, 9), pathSeg.P0);
    }

    [Test]
    public void PathSegmentAtIndex_ReturnsBezierSegment()
    {
        var points = new Vector2[7];
        for (int i = 0; i < points.Length; ++i)
            points[i] = Vector2.one * i;

        var path = new BezierPathSegment[] {
            new BezierPathSegment() { P0 = points[0], P1 = points[1], P2 = points[2] },
            new BezierPathSegment() { P0 = points[3], P1 = points[4], P2 = points[5] },
            new BezierPathSegment() { P0 = points[6] }
        };

        var seg0 = VectorUtils.PathSegmentAtIndex(path, 0);
        Assert.AreEqual(points[0], seg0.P0);
        Assert.AreEqual(points[1], seg0.P1);
        Assert.AreEqual(points[2], seg0.P2);
        Assert.AreEqual(points[3], seg0.P3);

        var seg1 = VectorUtils.PathSegmentAtIndex(path, 1);
        Assert.AreEqual(points[3], seg1.P0);
        Assert.AreEqual(points[4], seg1.P1);
        Assert.AreEqual(points[5], seg1.P2);
        Assert.AreEqual(points[6], seg1.P3);
    }

    [Test]
    public void SegmentsInPath_ReturnsAllSegmentsInPath()
    {
        var points = new Vector2[7];
        for (int i = 0; i < points.Length; ++i)
            points[i] = Vector2.one * i;

        var path = new BezierPathSegment[] {
            new BezierPathSegment() { P0 = points[0], P1 = points[1], P2 = points[2] },
            new BezierPathSegment() { P0 = points[3], P1 = points[4], P2 = points[5] },
            new BezierPathSegment() { P0 = points[6] }
        };

        var segs = VectorUtils.SegmentsInPath(path).ToList();
        Assert.AreEqual(2, segs.Count);

        Assert.AreEqual(points[0], segs[0].P0);
        Assert.AreEqual(points[1], segs[0].P1);
        Assert.AreEqual(points[2], segs[0].P2);
        Assert.AreEqual(points[3], segs[0].P3);

        Assert.AreEqual(points[3], segs[1].P0);
        Assert.AreEqual(points[4], segs[1].P1);
        Assert.AreEqual(points[5], segs[1].P2);
        Assert.AreEqual(points[6], segs[1].P3);
    }

    [Test]
    public void PathEndsPerfectlyMatch_ReturnsTrue_WhenEndsPerfectlyMatches()
    {
        // Should return false if there's less than 2 segments
        Assert.IsFalse(VectorUtils.PathEndsPerfectlyMatch(new BezierPathSegment[0]));

        var path = new BezierPathSegment[] {
            new BezierPathSegment() { P0 = Vector2.zero, P1 = new Vector2(-10, 10), P2 = new Vector2(10, 10) },
            new BezierPathSegment() { P0 = Vector2.zero }
        };

        Assert.IsTrue(VectorUtils.PathEndsPerfectlyMatch(path));

        path[1].P0 = Vector2.one;

        Assert.IsFalse(VectorUtils.PathEndsPerfectlyMatch(path));
    }

    [Test]
    public void MakeEllipse_MakesRoundedRectWithEllipseShape()
    {
        var radiuses = new Vector2(10.0f, 20.0f);
        var ellipse = new Shape();
        VectorUtils.MakeEllipseShape(ellipse, Vector2.zero, radiuses.x, radiuses.y);

        var segs = ellipse.Contours[0].Segments;
        Assert.AreEqual(20.0f, (segs[2].P0-segs[0].P0).magnitude);
        Assert.AreEqual(40.0f, (segs[3].P0-segs[1].P0).magnitude);
    }

    [Test]
    public void MakeCircle_MakesRoundedRectWithCircleShape()
    {
        var radius = 10.0f;
        var circle = new Shape();
        VectorUtils.MakeCircleShape(circle, Vector2.zero, radius);

        var segs = circle.Contours[0].Segments;
        Assert.AreEqual(20.0f, (segs[2].P0-segs[0].P0).magnitude);
        Assert.AreEqual(20.0f, (segs[3].P0-segs[1].P0).magnitude);
    }

    [Test]
    public void Bounds_ComputesBezierPathBounds()
    {
        var path = VectorUtils.MakeArc(Vector2.zero, 0.0f, Mathf.PI / 2, 1.0f);
        var bbox = VectorUtils.Bounds(path);

        Assert.AreEqual(0.0f, bbox.min.x, VectorUtils.Epsilon);
        Assert.AreEqual(0.0f, bbox.min.y, VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, bbox.max.x, VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, bbox.max.y, VectorUtils.Epsilon);
    }

    [Test]
    public void Bounds_ReturnsVerticesBoundingBox()
    {
        var bbox = VectorUtils.Bounds(new Vector2[] { Vector2.zero, Vector2.right, Vector2.one });
        Assert.AreEqual(Vector2.zero, bbox.min);
        Assert.AreEqual(Vector2.one, bbox.max);
    }

    [Test]
    public void MakeLine_ReturnsLineSegment()
    {
        var from = Vector2.zero;
        var to = Vector2.one;
        var seg = VectorUtils.MakeLine(from, to);

        // All segment points should lie on the same line
        var v = (to - from).normalized;
        Assert.AreEqual(1.0f, Vector2.Dot((seg.P1 - from).normalized, v), VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, Vector2.Dot((seg.P2 - from).normalized, v), VectorUtils.Epsilon);
    }

    [Test]
    public void QuadraticToCubic_ReturnsCubicSegment()
    {
        var seg = VectorUtils.QuadraticToCubic(
            new Vector2(0.0f, 0.0f),
            new Vector2(0.5f, 0.5f),
            new Vector2(1.0f, 0.0f)
        );

        Assert.AreEqual(new Vector2(0.0f, 0.0f), VectorUtils.Eval(seg, 0.0f));
        Assert.AreEqual(new Vector2(0.25f, 0.1875f), VectorUtils.Eval(seg, 0.25f));
        Assert.AreEqual(new Vector2(0.5f, 0.25f), VectorUtils.Eval(seg, 0.5f));
        Assert.AreEqual(new Vector2(0.75f, 0.1875f), VectorUtils.Eval(seg, 0.75f));
        Assert.AreEqual(new Vector2(1.0f, 0.0f), VectorUtils.Eval(seg, 1.0f));
    }

    [Test]
    public void MakePathLine_ReturnsLinePathSegment()
    {
        var from = Vector2.zero;
        var to = Vector2.one;
        var pathSeg = VectorUtils.MakePathLine(from, to);

        // All segment points should lie on the same line
        var v = (to - from).normalized;
        Assert.AreEqual(1.0f, Vector2.Dot((pathSeg[0].P1 - from).normalized, v), VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, Vector2.Dot((pathSeg[0].P2 - from).normalized, v), VectorUtils.Epsilon);
    }

    [Test]
    public void MakeArc_ReturnsArcSegment()
    {
        var path = VectorUtils.MakeArc(Vector2.zero, 0.0f, Mathf.PI / 2, 1.0f);
        Assert.AreEqual(2, path.Length);

        var seg = VectorUtils.PathSegmentAtIndex(path, 0);
        for (var t = 0.0f; t <= 1.0f; t += 0.1f)
        {
            Assert.AreEqual(1.0f, VectorUtils.Eval(seg, t).magnitude, 0.001f);
        }
    }

    [Test]
    public void MakeArc_MakesArcInClockwiseDirection()
    {
        var path = VectorUtils.MakeArc(Vector2.zero, 0.0f, Mathf.PI / 2, 1.0f);
        Assert.AreEqual(2, path.Length);
        Assert.AreEqual(0.0f, path[1].P0.x, VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, path[1].P0.y, VectorUtils.Epsilon);
    }

    [Test]
    public void MakeArc_ReturnsMultipleSegmentsWhenArcSpansMoreThanOneQuadrant()
    {
        var path = VectorUtils.MakeArc(Vector2.zero, 0.0f, Mathf.PI, 1.0f);
        Assert.AreEqual(3, path.Length);

        foreach (var seg in VectorUtils.SegmentsInPath(path))
        {
            for (var t = 0.0f; t <= 1.0f; t += 0.1f)
            {
                Assert.AreEqual(1.0f, VectorUtils.Eval(seg, t).magnitude, 0.001f);
            }
        }
    }

    [Test]
    public void FlipSegment_FlipsPointsOrder()
    {
        var points = new Vector2[4];
        for (int i = 0; i < points.Length; ++i)
            points[i] = Vector2.one * i;

        var seg = new BezierSegment() { P0 = points[0], P1 = points[1], P2 = points[2], P3 = points[3] };
        var flipped = VectorUtils.FlipSegment(seg);

        Assert.AreEqual(points[3], flipped.P0);
        Assert.AreEqual(points[2], flipped.P1);
        Assert.AreEqual(points[1], flipped.P2);
        Assert.AreEqual(points[0], flipped.P3);
    }

    [Test]
    public void Eval_EvaluatesPointOnBezierSegment()
    {
        var seg = VectorUtils.MakeLine(Vector2.zero, Vector2.one);
        var p = VectorUtils.Eval(seg, 0.5f);
        Assert.AreEqual(0.5f, p.x, VectorUtils.Epsilon);
        Assert.AreEqual(0.5f, p.y, VectorUtils.Epsilon);
    }

    [Test]
    public void EvalNormal_EvaluatesNormalOnBezierSegment()
    {
        var seg = VectorUtils.MakeLine(Vector2.zero, Vector2.right);
        var n = VectorUtils.EvalNormal(seg, 0.5f);
        Assert.AreEqual(0.0f, n.x, VectorUtils.Epsilon);
        Assert.AreEqual(1.0f, n.y, VectorUtils.Epsilon);
    }

    [Test]
    public void SegmentLength_ReturnSegmentLength()
    {
        var seg = VectorUtils.MakeLine(Vector2.zero, Vector2.one);
        Assert.AreEqual(Mathf.Sqrt(2), VectorUtils.SegmentLength(seg), VectorUtils.Epsilon);
    }

    [Test]
    public void SplitSegment_ReturnsTwoSegmentsSeparatedAtParameterT()
    {
        var seg = VectorUtils.MakeLine(Vector2.zero, Vector2.right);

        BezierSegment first;
        BezierSegment second;
        VectorUtils.SplitSegment(seg, 0.5f, out first, out second);

        Assert.AreEqual(Vector2.zero, first.P0);
        Assert.AreEqual(Vector2.right * 0.5f, first.P3);

        Assert.AreEqual(Vector2.right * 0.5f, second.P0);
        Assert.AreEqual(Vector2.right, second.P3);
    }

    [Test]
    public void SceneNodes_ReturnsAllSceneNodes()
    {
        var child1 = new SceneNode();
        var child2 = new SceneNode();
        var root = new SceneNode();
        root.Children = new System.Collections.Generic.List<SceneNode>(2);
        root.Children.Add(child1);
        root.Children.Add(child2);

        var nodes = VectorUtils.SceneNodes(root).ToList();
        Assert.IsTrue(nodes.Contains(child1));
        Assert.IsTrue(nodes.Contains(child2));
        Assert.IsTrue(nodes.Contains(root));
    }

    [Test]
    public void WorldTransformedSceneNodes_ReturnsTransformedSceneNodes()
    {
        var child = new SceneNode();
        child.Transform = Matrix2D.identity;

        var root = new SceneNode();
        root.Children = new System.Collections.Generic.List<SceneNode>(2);
        root.Children.Add(child);

        var transform = Matrix2D.Translate(new Vector2(1, 2));
        root.Transform = transform;

        var nodes = VectorUtils.WorldTransformedSceneNodes(root, null);
        foreach (var nodeWithTransform in nodes)
        {
            Assert.AreEqual(1.0f, nodeWithTransform.WorldTransform.m02);
            Assert.AreEqual(2.0f, nodeWithTransform.WorldTransform.m12);
        }
    }

    [Test]
    public void WorldTransformedSceneNodes_ComputesParent()
    {
        var child = new SceneNode();
        var parent = new SceneNode() { Children = new List<SceneNode> { child } };

        bool childFound = false;

        var nodes = VectorUtils.WorldTransformedSceneNodes(parent, null);
        foreach (var nodeWithTransform in nodes)
        {
            if (nodeWithTransform.Node == child)
            {
                childFound = true;
                Assert.AreEqual(parent, nodeWithTransform.Parent);
                break;
            }
        }

        Assert.IsTrue(childFound);
    }

    [Test]
    public void IntersectLines_ReturnsLineIntersection()
    {
        var intersect0 = VectorUtils.IntersectLines(Vector2.zero, Vector2.one, new Vector2(0, 1), new Vector2(1, 0));
        Assert.AreEqual(Vector2.one * 0.5f, intersect0);

        var intersect1 = VectorUtils.IntersectLines(Vector2.zero, Vector2.one, Vector2.zero, Vector2.one);
        Assert.IsTrue(float.IsInfinity(intersect1.x));
        Assert.IsTrue(float.IsInfinity(intersect1.y));
    }

    [Test]
    public void IntersectLineSegments_ReturnsLineIntersection_OnlyWhenTouching()
    {
        var intersect0 = VectorUtils.IntersectLineSegments(Vector2.zero, Vector2.one, new Vector2(0, 1), new Vector2(1, 0));
        Assert.AreEqual(Vector2.one * 0.5f, intersect0);

        var intersect1 = VectorUtils.IntersectLineSegments(Vector2.zero, Vector2.one * 0.25f, new Vector2(0, 1), new Vector2(1, 0));
        Assert.IsTrue(float.IsInfinity(intersect1.x));
        Assert.IsTrue(float.IsInfinity(intersect1.y));
    }

    [Test]
    public void FindBezierLineIntersections_ReturnsBezierLineIntersections()
    {
        var path = VectorUtils.MakeArc(Vector2.zero, 0.0f, Mathf.PI / 2, 1.0f);
        var seg = VectorUtils.PathSegmentAtIndex(path, 0);
        var ts = VectorUtils.FindBezierLineIntersections(seg, new Vector2(0.0f , 1.1f), new Vector2(1.1f, 0.0f));

        Assert.AreEqual(2, ts.Length);

        var p = VectorUtils.Eval(seg, ts[0]);
        Assert.AreEqual(0.995f, p.x, 0.001f);
        Assert.AreEqual(0.096f, p.y, 0.001f);

        var q = VectorUtils.Eval(seg, ts[1]);
        Assert.AreEqual(0.096f, q.x, 0.001f);
        Assert.AreEqual(0.995f, q.y, 0.001f);
    }

    [Test]
    public void FillMesh_WithFlipYAxis_FlipsYAxis()
    {
        // Build a square of size 1x1 located at (2,2)
        var rectShape = new Shape();
        VectorUtils.MakeRectangleShape(rectShape, new Rect(2, 2, 1, 1));
        rectShape.Fill = new SolidFill() { Color = Color.red };
        var scene = new Scene() {
            Root = new SceneNode() { Shapes = new List<Shape>() { rectShape } }
        };

        var options = new VectorUtils.TessellationOptions() {
            StepDistance = 1000.0f,
            MaxCordDeviation = float.MaxValue,
            MaxTanAngleDeviation = float.MaxValue,
            SamplingStepSize = 0.01f
        };
        var geoms = VectorUtils.TessellateScene(scene, options);

        // Build a mesh without flipping
        var mesh = new Mesh();
        VectorUtils.FillMesh(mesh, geoms, 1.0f, false);

        // Build a mesh with flipping
        var flippedMesh = new Mesh();
        VectorUtils.FillMesh(flippedMesh, geoms, 1.0f, true);

        for (int i = 0; i < mesh.vertices.Length; ++i)
        {
            var vert = mesh.vertices[i];
            var flippedVert = flippedMesh.vertices[i];
            float expectedY = (1.0f - (vert.y-2.0f)) + 2.0f; // Manual flip for the test!
            Assert.AreEqual(expectedY, flippedVert.y, 0.001f);
        }
    }
    
    [Test]
    public void VectorExpandEdges_ShaderExistsInPath()
    {
        // The following file is used in VectorSprite.cs
        Assert.IsTrue(System.IO.File.Exists("Packages/com.unity.vectorgraphics/Runtime/Shaders/VectorExpandEdges.shader"));
    }
}
