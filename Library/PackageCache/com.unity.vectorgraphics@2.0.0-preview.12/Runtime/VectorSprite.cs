using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;
using Unity.Collections;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.U2D;
#if !(UNITY_2019_3_OR_NEWER)
using UnityEngine.Experimental.U2D;
#endif
using UnityEngine.Experimental.Rendering;

namespace Unity.VectorGraphics
{
    public static partial class VectorUtils
    {
        /// <summary>The alignement of the sprite, to determine the location of the pivot.</summary>
        public enum Alignment
        {
            /// <summary>Center alignment.</summary>
            Center = 0,

            /// <summary>Top-left alignment.</summary>
            TopLeft = 1,

            /// <summary>Top-center alignment.</summary>
            TopCenter = 2,

            /// <summary>Top-right alignment.</summary>
            TopRight = 3,

            /// <summary>Left-center alignment.</summary>
            LeftCenter = 4,

            /// <summary>Right-center alignment.</summary>
            RightCenter = 5,

            /// <summary>Bottom-left alignment.</summary>
            BottomLeft = 6,

            /// <summary>Bottom-center alignment.</summary>
            BottomCenter = 7,

            /// <summary>Bottom-right alignment.</summary>
            BottomRight = 8,

            /// <summary>Custom alignment.</summary>
            /// <remarks>
            /// Uses a custom alignment that will be used when building the sprite using the <see cref="BuildSprite"/> method.
            /// </remarks>
            Custom = 9,

            /// <summary>SVG origin alignment.</summary>
            /// <remarks>
            /// This will use the origin of the SVG document as the origin of the sprite.
            /// </remarks>
            SVGOrigin = 10
        }

        /// <summary>Builds a sprite asset from a scene tessellation.</summary>
        /// <param name="geoms">The list of tessellated Geometry instances</param>
        /// <param name="svgPixelsPerUnit">How many SVG "pixels" map into a Unity unit</param>
        /// <param name="alignment">The position of the sprite origin</param>
        /// <param name="customPivot">If alignment is <see cref="Alignment.Custom"/>, customPivot is used to compute the sprite origin</param>
        /// <param name="gradientResolution">The maximum size of the texture holding gradient data</param>
        /// <param name="flipYAxis">True to have the positive Y axis to go downward.</param>
        /// <returns>A new Sprite containing the provided geometry. The Sprite may have a texture if the geometry has any texture and/or gradients</returns>
        public static Sprite BuildSprite(List<Geometry> geoms, float svgPixelsPerUnit, Alignment alignment, Vector2 customPivot, UInt16 gradientResolution, bool flipYAxis = false)
        {
            return BuildSprite(geoms, Rect.zero, svgPixelsPerUnit, alignment, customPivot, gradientResolution, flipYAxis);
        }

        /// <summary>Builds a sprite asset from a scene tessellation.</summary>
        /// <param name="geoms">The list of tessellated Geometry instances</param>
        /// <param name="rect">The position and size of the sprite geometry</param>
        /// <param name="svgPixelsPerUnit">How many SVG "pixels" map into a Unity unit</param>
        /// <param name="alignment">The position of the sprite origin</param>
        /// <param name="customPivot">If alignment is <see cref="Alignment.Custom"/>, customPivot is used to compute the sprite origin</param>
        /// <param name="gradientResolution">The maximum size of the texture holding gradient data</param>
        /// <param name="flipYAxis">True to have the positive Y axis to go downward.</param>
        /// <returns>A new Sprite containing the provided geometry. The Sprite may have a texture if the geometry has any texture and/or gradients</returns>
        public static Sprite BuildSprite(List<Geometry> geoms, Rect rect, float svgPixelsPerUnit, Alignment alignment, Vector2 customPivot, UInt16 gradientResolution, bool flipYAxis = false)
        {
            // Generate atlas
            var texAtlas = GenerateAtlasAndFillUVs(geoms, gradientResolution);

            List<Vector2> vertices;
            List<UInt16> indices;
            List<Color> colors;
            List<Vector2> uvs;
            List<Vector2> settingIndices;
            FillVertexChannels(geoms, 1.0f, texAtlas != null, out vertices, out indices, out colors, out uvs, out settingIndices, flipYAxis);

            Texture2D texture = texAtlas != null ? texAtlas.Texture : null;

            if (rect == Rect.zero)
            {
                rect = VectorUtils.Bounds(vertices);
                VectorUtils.RealignVerticesInBounds(vertices, rect, flipYAxis);
            }
            else if (flipYAxis)
            {
                VectorUtils.FlipVerticesInBounds(vertices, rect);

                // The provided rect should normally contain the whole geometry, but since VectorUtils.SceneNodeBounds doesn't
                // take the strokes into account, some triangles may appear outside the rect. We clamp the vertices as a workaround for now.
                VectorUtils.ClampVerticesInBounds(vertices, rect);
            }

            var pivot = GetPivot(alignment, customPivot, rect, flipYAxis);

            // The Sprite.Create(Rect, Vector2, float, Texture2D) method is internal. Using reflection
            // until it becomes public.
            var spriteCreateMethod = typeof(Sprite).GetMethod("Create", BindingFlags.Static | BindingFlags.NonPublic, Type.DefaultBinder, new Type[] { typeof(Rect), typeof(Vector2), typeof(float), typeof(Texture2D) }, null);
            var sprite = spriteCreateMethod.Invoke(null, new object[] { rect, pivot, svgPixelsPerUnit, texture }) as Sprite;

            sprite.OverrideGeometry(vertices.ToArray(), indices.ToArray());

            if (colors != null)
            {
                var colors32 = colors.Select(c => (Color32)c);
                using (var nativeColors = new NativeArray<Color32>(colors32.ToArray(), Allocator.Temp))
                    sprite.SetVertexAttribute<Color32>(VertexAttribute.Color, nativeColors);
            }
            if (uvs != null)
            {
                using (var nativeUVs = new NativeArray<Vector2>(uvs.ToArray(), Allocator.Temp))
                    sprite.SetVertexAttribute<Vector2>(VertexAttribute.TexCoord0, nativeUVs);
                using (var nativeSettingIndices = new NativeArray<Vector2>(settingIndices.ToArray(), Allocator.Temp))
                    sprite.SetVertexAttribute<Vector2>(VertexAttribute.TexCoord2, nativeSettingIndices);
            }

            return sprite;
        }

        /// <summary>Fills a mesh geometry from a scene tessellation.</summary>
        /// <param name="mesh">The mesh object to fill</param>
        /// <param name="geoms">The list of tessellated Geometry instances, generated by TessellateNodeHierarchy</param>
        /// <param name="svgPixelsPerUnit">How many SVG "pixels" map into a Unity unit</param>
        /// <param name="flipYAxis">Set to "true" to have the positive Y axis to go downward.</param>
        public static void FillMesh(Mesh mesh, List<Geometry> geoms, float svgPixelsPerUnit, bool flipYAxis = false)
        {
            bool hasUVs = (geoms.FirstOrDefault(g => g.UVs != null)) != null;

            // Generate atlas
            List<Vector2> vertices;
            List<UInt16> indices;
            List<Color> colors;
            List<Vector2> uvs;
            List<Vector2> settingIndices;
            FillVertexChannels(geoms, svgPixelsPerUnit, hasUVs, out vertices, out indices, out colors, out uvs, out settingIndices, flipYAxis);

            if (flipYAxis)
                FlipYAxis(vertices);

            mesh.Clear();
            mesh.SetVertices(vertices.Select(v => (Vector3)v).ToList());
            mesh.SetTriangles(indices.Select(i => (int)i).ToArray(), 0);

            if (colors != null)
                mesh.SetColors(colors);

            if (uvs != null)
                mesh.SetUVs(0, uvs);
            if (settingIndices  != null)
                mesh.SetUVs(2, settingIndices);
        }

        private static void FlipYAxis(IList<Vector2> vertices)
        {
            var bbox = Bounds(vertices);
            var h = bbox.height;
            for (int i = 0; i < vertices.Count; ++i)
            {
                var v = vertices[i];
                v.y -= bbox.position.y;
                v.y = h - v.y;
                v.y += bbox.position.y;
                vertices[i] = v;
            }
        }

        private static void FillVertexChannels(List<Geometry> geoms, float pixelsPerUnit, bool hasUVs, out List<Vector2> vertices, out List<UInt16> indices, out List<Color> colors, out List<Vector2> uvs, out List<Vector2> settingIndices, bool flipYAxis)
        {
            int totalVerts = 0, totalIndices = 0;
            foreach (var geom in geoms)
            {
                if (geom.Indices.Length != 0)
                {
                    totalIndices += geom.Indices.Length;
                    totalVerts += geom.Vertices.Length;
                }
            }

            vertices = new List<Vector2>(totalVerts);
            indices = new List<UInt16>(totalIndices);
            colors = new List<Color>(totalVerts);
            uvs = hasUVs ? new List<Vector2>(totalVerts) : null;
            settingIndices = hasUVs ? new List<Vector2>(totalVerts) : null;

            foreach (var geom in geoms)
            {
                int indexStart = indices.Count;
                int indexEnd = indexStart + geom.Indices.Length;

                int vertexCount = vertices.Count;
                indices.AddRange(geom.Indices.Select(x => (UInt16)(x + vertexCount)));
                vertices.AddRange(geom.Vertices.Select(x => (geom.WorldTransform * x) / pixelsPerUnit));
                colors.AddRange(Enumerable.Repeat(geom.Color, geom.Vertices.Length));

                FlipRangeIfNecessary(vertices, indices, indexStart, indexEnd, flipYAxis);

                System.Diagnostics.Debug.Assert(uvs == null || geom.UVs != null);
                if (uvs != null)
                {
                    uvs.AddRange(geom.UVs);
                    for (int i = 0; i < geom.UVs.Length; i++)
                        settingIndices.Add(new Vector2(geom.SettingIndex, 0));
                }
            }
        }

        internal enum WindingDir
        {
            CW,
            CCW
        }

        internal static void AdjustWinding(Vector2[] vertices, UInt16[] indices, WindingDir dir)
        {
            bool shouldFlip = false;
            var length = indices.Length;
            for (int i = 0; i < (length - 2); i += 3)
            {
                var v0 = (Vector3)vertices[indices[i]];
                var v1 = (Vector3)vertices[indices[i+1]];
                var v2 = (Vector3)vertices[indices[i+2]];
                var s = (v1 - v0).normalized;
                var t = (v2 - v0).normalized;
                float dot = Vector3.Dot(s, t);
                if (s == Vector3.zero || t == Vector3.zero || dot > 0.9999f || dot < -0.9999f)
                    continue;
                var n = Vector3.Cross(s, t);
                if (n.sqrMagnitude < 0.0001f)
                    continue;
                shouldFlip = dir == WindingDir.CCW ? n.z < 0.0f : n.z > 0.0f;
                break;
            }
            if (shouldFlip)
            {
                for (int i = 0; i < (length - 2); i += 3)
                {
                    var tmp = indices[i];
                    indices[i] = indices[i+1];
                    indices[i+1] = tmp;
                }
            }
        }

        private static void FlipRangeIfNecessary(List<Vector2> vertices, List<UInt16> indices, int indexStart, int indexEnd, bool flipYAxis)
        {
            // For the range, find the first valid triangle and check its winding order. If that triangle needs flipping, then flip the whole range.
            bool shouldFlip = false;
            for (int i = indexStart; i < (indexEnd - 2); i += 3)
            {
                var v0 = (Vector3)vertices[indices[i]];
                var v1 = (Vector3)vertices[indices[i + 1]];
                var v2 = (Vector3)vertices[indices[i + 2]];
                var s = (v1 - v0).normalized;
                var t = (v2 - v0).normalized;
                float dot = Vector3.Dot(s, t);
                if (s == Vector3.zero || t == Vector3.zero || dot > 0.99f || dot < -0.99f)
                    continue;
                var n = Vector3.Cross(s, t);
                if (n.sqrMagnitude < 0.001f)
                    continue;
                shouldFlip = flipYAxis ? n.z < 0.0f : n.z > 0.0f;
                break;
            }
            if (shouldFlip)
            {
                for (int i = indexStart; i < (indexEnd - 2); i += 3)
                {
                    var tmp = indices[i + 1];
                    indices[i + 1] = indices[i + 2];
                    indices[i + 2] = tmp;
                }
            }
        }

        internal static void RenderFromArrays(Vector2[] vertices, UInt16[] indices, Vector2[] uvs, Color[] colors, Vector2[] settings, Texture2D texture, Material mat, bool clear = true)
        {
            mat.SetTexture("_MainTex", texture);
            mat.SetPass(0);

            if (clear)
                GL.Clear(true, true, Color.clear);

            GL.PushMatrix();
            GL.LoadOrtho();
            GL.Color(new Color(1, 1, 1, 1));
            GL.Begin(GL.TRIANGLES);
            for (int i = 0; i < indices.Length; ++i)
            {
                ushort index = indices[i];
                Vector2 vertex = vertices[index];
                Vector2 uv = uvs[index];
                GL.TexCoord2(uv.x, uv.y);
                if (settings != null)
                {
                    var setting = settings[index];
                    GL.MultiTexCoord2(2, setting.x, setting.y);
                }
                if (colors != null)
                    GL.Color(colors[index]);
                GL.Vertex3(vertex.x, vertex.y, 0);
            }
            GL.End();
            GL.PopMatrix();

            mat.SetTexture("_MainTex", null);
        }

        /// <summary>Draws a vector sprite using the provided material.</summary>
        /// <param name="sprite">The sprite to render</param>
        /// <param name="mat">The material used for rendering</param>
        /// <param name="clear">If true, clear the render target before rendering</param>
        public static void RenderSprite(Sprite sprite, Material mat, bool clear = true) 
        {
            float spriteWidth = sprite.rect.width;
            float spriteHeight = sprite.rect.height;
            float pixelsToUnits = sprite.rect.width / sprite.bounds.size.x;

            var uvs = sprite.uv;
            var triangles = sprite.triangles;
            var pivot = sprite.pivot;

            var vertices = sprite.vertices.Select(v => 
                new Vector2((v.x * pixelsToUnits + pivot.x)/spriteWidth,
                            (v.y * pixelsToUnits + pivot.y)/spriteHeight)
            ).ToArray();

            Color[] colors = null;
            if (sprite.HasVertexAttribute(VertexAttribute.Color))
                colors = sprite.GetVertexAttribute<Color32>(VertexAttribute.Color).Select(c => (Color)c).ToArray();

            Vector2[] settings = null;
            if (sprite.HasVertexAttribute(VertexAttribute.TexCoord2))
                settings = sprite.GetVertexAttribute<Vector2>(VertexAttribute.TexCoord2).ToArray();

            RenderFromArrays(vertices, sprite.triangles, sprite.uv, colors, settings, sprite.texture, mat, clear);
        }

        private static Material s_ExpandEdgesMat;

        /// <summary>Renders a vector sprite to Texture2D.</summary>
        /// <param name="sprite">The sprite to render</param>
        /// <param name="width">The desired width of the resulting texture</param>
        /// <param name="height">The desired height of the resulting texture</param>
        /// <param name="mat">The material used to render the sprite</param>
        /// <param name="antiAliasing">The number of samples per pixel for anti-aliasing</param>
        /// <param name="expandEdges">When true, expand the edges to avoid a dark banding effect caused by filtering. This is slower to render and uses more graphics memory.</param>
        /// <returns>A Texture2D object containing the rendered vector sprite</returns>
        public static Texture2D RenderSpriteToTexture2D(Sprite sprite, int width, int height, Material mat, int antiAliasing = 1, bool expandEdges = false)
        {
            if (width <= 0 || height <= 0)
                return null;

            RenderTexture tex = null;
            var oldActive = RenderTexture.active;

            var desc = new RenderTextureDescriptor(width, height, RenderTextureFormat.ARGB32, 0) {
                msaaSamples = 1,
                sRGB = QualitySettings.activeColorSpace == ColorSpace.Linear
            };

            if (expandEdges)
            {
                // Draw the sprite normally to be used as a background, no-antialiasing
                var normalTex = RenderTexture.GetTemporary(desc);
                RenderTexture.active = normalTex;
                RenderSprite(sprite, mat);

                // Expand the edges and make completely transparent
                if (s_ExpandEdgesMat == null)
                {
                    var shader = Shader.Find("Hidden/VectorExpandEdges");
                    if (shader == null)
                    {
#if UNITY_EDITOR
                        // Workaround for case 1167309.
                        // Shader.Find() seems to fail on the package shader when doing a fresh import with a clean Library folder,
                        // but AssetDatabase.LoadAssetAtPath() works fine though.
                        shader = UnityEditor.AssetDatabase.LoadAssetAtPath<Shader>("Packages/com.unity.vectorgraphics/Runtime/Shaders/VectorExpandEdges.shader");
#else
                        return null;
#endif
                    }
                    s_ExpandEdgesMat = new Material(shader);
                }

                var expandTex = RenderTexture.GetTemporary(desc);
                RenderTexture.active = expandTex;
                GL.Clear(false, true, Color.clear);
                Graphics.Blit(normalTex, expandTex, s_ExpandEdgesMat, 0);
                RenderTexture.ReleaseTemporary(normalTex);

                // Draw the sprite again, but clear with the texture rendered in the previous step,
                // this will make the bilinear filter to interpolate the colors with values different
                // than "transparent black", which causes black-ish outlines around the shape.
                desc.msaaSamples = antiAliasing;
                tex = RenderTexture.GetTemporary(desc);
                RenderTexture.active = tex;
                Graphics.Blit(expandTex, tex);
                RenderTexture.ReleaseTemporary(expandTex); // Use the expanded texture to clear the buffer

                RenderTexture.active = tex;
                RenderSprite(sprite, mat, false);
            }
            else
            {
                desc.msaaSamples = antiAliasing;
                tex = RenderTexture.GetTemporary(desc);
                RenderTexture.active = tex;
                RenderSprite(sprite, mat);
            }

            Texture2D copy = new Texture2D(width, height, TextureFormat.RGBA32, false);
            copy.hideFlags = HideFlags.HideAndDontSave;
            copy.ReadPixels(new Rect(0, 0, width, height), 0, 0);
            copy.Apply();

            RenderTexture.active = oldActive;
            RenderTexture.ReleaseTemporary(tex);

            return copy;
        }

        internal static Vector2 GetPivot(Alignment alignment, Vector2 customPivot, Rect bbox, bool flipYAxis)
        {
            switch (alignment)
            {
                case Alignment.Center: return new Vector2(0.5f, 0.5f);
                case Alignment.TopLeft: return new Vector2(0.0f, 1.0f);
                case Alignment.TopCenter: return new Vector2(0.5f, 1.0f);
                case Alignment.TopRight: return new Vector2(1.0f, 1.0f);
                case Alignment.LeftCenter: return new Vector2(0.0f, 0.5f);
                case Alignment.RightCenter: return new Vector2(1.0f, 0.5f);
                case Alignment.BottomLeft: return new Vector2(0.0f, 0.0f);
                case Alignment.BottomCenter: return new Vector2(0.5f, 0.0f);
                case Alignment.BottomRight: return new Vector2(1.0f, 0.0f);
                case Alignment.SVGOrigin: 
                {
                     var p = -bbox.position / bbox.size;
                     if (flipYAxis)
                        p.y = 1.0f - p.y;
                    return p;
                }
                case Alignment.Custom: return customPivot;
            }
            return Vector2.zero;
        }
    }
}
