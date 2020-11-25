using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using UnityEngine.UIElements;

namespace Unity.VectorGraphics
{
#if UNITY_2019_3_OR_NEWER
    internal static class InternalBridge
    {
        // UnityEngine.UIElements.VectorImage is internal, so this method needs to be part of the internal bridge for now.
        internal static void MakeVectorImageAsset(IEnumerable<VectorUtils.Geometry> geoms, uint rasterSize, out UnityEngine.Object outAsset, out Texture2D outTexAtlas)
        {
            var atlas = VectorUtils.GenerateAtlas(geoms, rasterSize, false, false);
            if (atlas != null)
                VectorUtils.FillUVs(geoms, atlas);

            bool hasTexture = atlas != null && atlas.Texture != null;
            outTexAtlas = hasTexture ? atlas.Texture : null;

            var vertices = new List<VectorImageVertex>(100);
            var indices = new List<UInt16>(300);
            var settings = new List<GradientSettings>();

            var min = new Vector2(float.MaxValue, float.MaxValue);
            var max = new Vector2(float.MinValue, float.MinValue);
            foreach (var geom in geoms)
            {
                if (geom.Vertices.Length == 0)
                    continue;
                var b = VectorUtils.Bounds(geom.Vertices.Select(v => geom.WorldTransform.MultiplyPoint(v)));
                min = Vector2.Min(min, b.min);
                max = Vector2.Max(max, b.max);
            }
            var bounds = Rect.zero;
            if (min.x != float.MaxValue)
                bounds = new Rect(min, max-min);

            // Save written settings to avoid duplicates
            var writtenSettings = new HashSet<int>();
            writtenSettings.Add(0);

            // Create a map of filling -> atlas entry
            var fillEntries = new Dictionary<IFill, VectorUtils.PackRectItem>();
            if (atlas != null && atlas.Entries != null)
            {
                foreach (var entry in atlas.Entries)
                {
                    if (entry.Fill != null)
                        fillEntries[entry.Fill] = entry;
                }
            }

            if (hasTexture && atlas != null && atlas.Entries != null && atlas.Entries.Count > 0)
            {
                // Write the 'white' texel info
                var entry = atlas.Entries[atlas.Entries.Count-1];
                settings.Add(new GradientSettings() {
                    gradientType = UnityEngine.UIElements.GradientType.Linear,
                    addressMode = UnityEngine.UIElements.AddressMode.Wrap,
                    radialFocus = Vector2.zero,
                    location = new RectInt((int)entry.Position.x, (int)entry.Position.y, (int)entry.Size.x, (int)entry.Size.y)
                });
            }

            foreach (var geom in geoms)
            {
                for (int i = 0; i < geom.Vertices.Length; ++i)
                {
                    var v = geom.WorldTransform.MultiplyPoint(geom.Vertices[i]);
                    v -= bounds.position;
                    geom.Vertices[i] = v;
                }

                VectorUtils.AdjustWinding(geom.Vertices, geom.Indices, VectorUtils.WindingDir.CCW);

                var count = vertices.Count;
                for (int i = 0; i < geom.Vertices.Length; ++i)
                {
                    Vector3 p = (Vector3)geom.Vertices[i];
                    p.z = Vertex.nearZ;
                    vertices.Add(new VectorImageVertex() {
                        position = p,
                        uv = hasTexture ? geom.UVs[i] : Vector2.zero,
                        tint = geom.Color,
                        settingIndex = (uint)geom.SettingIndex
                    });
                }

                indices.AddRange(geom.Indices.Select(i => (UInt16)(i + count)));

                if (atlas != null && atlas.Entries != null && atlas.Entries.Count > 0)
                {
                    VectorUtils.PackRectItem entry;
                    if (geom.Fill == null || !fillEntries.TryGetValue(geom.Fill, out entry) || writtenSettings.Contains(entry.SettingIndex))
                        continue;
                    
                    writtenSettings.Add(entry.SettingIndex);

                    var gradientType = GradientFillType.Linear;
                    var radialFocus = Vector2.zero;
                    var addressMode = AddressMode.Wrap;

                    var gradientFill = geom.Fill as GradientFill;
                    if (gradientFill != null)
                    {
                        gradientType = gradientFill.Type;
                        radialFocus = gradientFill.RadialFocus;
                        addressMode = gradientFill.Addressing;
                    }
                    
                    var textureFill= geom.Fill as TextureFill;
                    if (textureFill != null)
                        addressMode = textureFill.Addressing;
                    
                    settings.Add(new GradientSettings() {
                        gradientType = (UnityEngine.UIElements.GradientType)gradientType,
                        addressMode = (UnityEngine.UIElements.AddressMode)addressMode,
                        radialFocus = radialFocus,
                        location = new RectInt((int)entry.Position.x, (int)entry.Position.y, (int)entry.Size.x, (int)entry.Size.y)
                    });
                }
            }

            var vectorImage = ScriptableObject.CreateInstance<VectorImage>();
            vectorImage.vertices = vertices.ToArray();
            vectorImage.indices = indices.ToArray();
            vectorImage.atlas = outTexAtlas;
            vectorImage.settings = settings.ToArray();
            vectorImage.size = bounds.size;

            outAsset = vectorImage;
        }

        internal static bool GetDataFromVectorImage(UnityEngine.Object o, ref Vector2[] vertices, ref UInt16[] indices, ref Vector2[] uvs, ref Color[] colors, ref Vector2[] settingIndices, ref Texture2D texture, ref Vector2 size)
        {
            var vi = o as VectorImage;
            if (vi == null)
                return false;
            
            vertices = vi.vertices.Select(v => (Vector2)v.position).ToArray();
            indices = vi.indices;
            uvs = vi.vertices.Select(v => v.uv).ToArray();
            colors = vi.vertices.Select(v => (Color)v.tint).ToArray();
            settingIndices = vi.atlas != null ? vi.vertices.Select(v => new Vector2(v.settingIndex, 0)).ToArray() : null;
            texture = vi.atlas;
            size = vi.size;

            return true;
        }

        internal static Texture2D RenderVectorImageToTexture2D(UnityEngine.Object o, int width, int height, Material mat, int antiAliasing = 1)
        {
            var vi = o as VectorImage;
            if (o == null)
                return null;

            if (width <= 0 || height <= 0)
                return null;

            RenderTexture rt = null;
            var oldActive = RenderTexture.active;

            var desc = new RenderTextureDescriptor(width, height, RenderTextureFormat.ARGB32, 0) {
                msaaSamples = antiAliasing,
                sRGB = QualitySettings.activeColorSpace == ColorSpace.Linear
            };

            rt = RenderTexture.GetTemporary(desc);
            RenderTexture.active = rt;
            
            Vector2[] vertices = null;
            UInt16[] indices = null;
            Vector2[] uvs = null;
            Color[] colors = null;
            Vector2[] settingIndices = null;
            Texture2D atlas = null;
            Vector2 size = Vector2.zero;
            if (InternalBridge.GetDataFromVectorImage(o, ref vertices, ref indices, ref uvs, ref colors, ref settingIndices, ref atlas, ref size))
            {
                vertices = vertices.Select(v => new Vector2(v.x/size.x, 1.0f-v.y/size.y)).ToArray();
                var atlasWithEncodedSettings = vi.atlas != null ? BuildAtlasWithEncodedSettings(vi.settings, vi.atlas) : null;
                VectorUtils.RenderFromArrays(vertices, indices, uvs, colors, settingIndices, atlasWithEncodedSettings, mat);
                Texture2D.DestroyImmediate(atlasWithEncodedSettings);
            }
            else
            {
                RenderTexture.active = oldActive;
                RenderTexture.ReleaseTemporary(rt);
                return null;
            }

            Texture2D copy = new Texture2D(width, height, TextureFormat.RGBA32, false);
            copy.hideFlags = HideFlags.HideAndDontSave;
            copy.ReadPixels(new Rect(0, 0, width, height), 0, 0);
            copy.Apply();

            RenderTexture.active = oldActive;
            RenderTexture.ReleaseTemporary(rt);

            return copy;
        }

        private static Texture2D BuildAtlasWithEncodedSettings(GradientSettings[] settings, Texture2D atlas)
        {
            var oldActive = RenderTexture.active;

            int width = atlas.width + 3;
            int height = Math.Max(settings.Length, atlas.height);

            var desc = new RenderTextureDescriptor(width, height, RenderTextureFormat.ARGB32, 0) {
                sRGB = QualitySettings.activeColorSpace == ColorSpace.Linear
            };
            var rt = RenderTexture.GetTemporary(desc);
            GL.Clear(false, true, Color.black, 1.0f);
            Graphics.Blit(atlas, rt, Vector2.one, new Vector2(-3.0f/width, 0.0f));
            RenderTexture.active = rt;
            
            Texture2D copy = new Texture2D(width, height, TextureFormat.RGBA32, false);
            copy.hideFlags = HideFlags.HideAndDontSave;
            copy.ReadPixels(new Rect(0, 0, width, height), 0, 0);

            // This encoding procedure is duplicated a few times, do something about it
            var rawSettingsTex = new VectorUtils.RawTexture() {
                Width = 3, Height = settings.Length,
                Rgba = new Color32[3*settings.Length]
            };
            for (int i = 0; i < settings.Length; ++i)
            {
                var g = settings[i];

                // There are 3 consecutive pixels to store the settings
                int destX = 0;
                int destY = i;

                if (g.gradientType == GradientType.Radial)
                {
                    var focus = g.radialFocus;
                    focus += Vector2.one;
                    focus /= 2.0f;
                    focus.y = 1.0f - focus.y;

                    VectorUtils.WriteRawFloat4Packed(rawSettingsTex, ((float)g.gradientType)/255, ((float)g.addressMode)/255, focus.x, focus.y, destX++, destY);
                }
                else
                {
                    VectorUtils.WriteRawFloat4Packed(rawSettingsTex, 0.0f, ((float)g.addressMode)/255, 0.0f, 0.0f, destX++, destY);
                }

                var pos = g.location.position;
                var size = g.location.size;
                size.x -= 1;
                size.y -= 1;
                VectorUtils.WriteRawInt2Packed(rawSettingsTex, (int)pos.x+3, (int)pos.y, destX++, destY);
                VectorUtils.WriteRawInt2Packed(rawSettingsTex, (int)size.x, (int)size.y, destX++, destY);
            }

            copy.SetPixels32(0, 0, 3, settings.Length, rawSettingsTex.Rgba, 0);
            copy.Apply();
            
            RenderTexture.active = oldActive;
            RenderTexture.ReleaseTemporary(rt);

            return copy;
        }
    }
#endif
}