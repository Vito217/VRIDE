using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using Unity.Collections;
using UnityEngine;
using UnityEditor;
using UnityEngine.Experimental.U2D;
#if !(UNITY_2019_3_OR_NEWER)
using UnityEditor.Experimental.U2D;
#endif
using UnityEditor.U2D.Sprites;

namespace Unity.VectorGraphics.Editor
{
    [Serializable]
    internal class SVGSpriteData
    {
        public float TessellationDetail = 0.0f;
        public SpriteRect SpriteRect = new SpriteRect();
        public List<OutlineData> PhysicsOutlines = new List<OutlineData>();

        private SpriteAlignment m_PrevAlignment;
        private Vector2 m_PrevPivot;

        public void Load(SerializedObject so)
        {
            var importer = so.targetObject as SVGImporter;
            var sprite = SVGImporter.GetImportedSprite(importer.assetPath);
            if (sprite == null)
                return;

            SpriteRect.name = sprite.name;

            int targetWidth;
            int targetHeight;
            importer.TextureSizeForSpriteEditor(sprite, out targetWidth, out targetHeight);
            SpriteRect.rect = new Rect(0, 0, targetWidth, targetHeight);
            var textureSize = new Vector2(targetWidth, targetHeight);

            var baseSP = so.FindProperty("m_SpriteData");
            var spriteRectSP = baseSP.FindPropertyRelative("SpriteRect");
            SpriteRect.border = spriteRectSP.FindPropertyRelative("m_Border").vector4Value;
            SpriteRect.pivot = sprite.pivot / textureSize;

            var guidSP = spriteRectSP.FindPropertyRelative("m_SpriteID");
            SpriteRect.spriteID = new GUID(guidSP.stringValue);

            SpriteRect.alignment = SpriteAlignment.Center;
            if (Enum.IsDefined(typeof(SpriteAlignment), (int)importer.Alignment))
                SpriteRect.alignment = (SpriteAlignment)importer.Alignment;
            else if (importer.Alignment == VectorUtils.Alignment.SVGOrigin)
                SpriteRect.alignment = SpriteAlignment.Custom;
            m_PrevAlignment = SpriteRect.alignment;
            m_PrevPivot = SpriteRect.pivot;
        }

        public void Apply(SerializedObject so)
        {
            if (SpriteRect.alignment != m_PrevAlignment || SpriteRect.pivot != m_PrevPivot)
            {
                // Only apply the alignment if it changed, otherwise we may override the special "SVG Origin" value
                var alignSP = so.FindProperty("m_Alignment");
                alignSP.intValue = (int)SpriteRect.alignment;

                var pivotSP = so.FindProperty("m_CustomPivot");
                pivotSP.vector2Value = SpriteRect.pivot;
            }

            var baseSP = so.FindProperty("m_SpriteData");
            var spriteRectSP = baseSP.FindPropertyRelative("SpriteRect");
            var borderSP = spriteRectSP.FindPropertyRelative("m_Border");
            borderSP.vector4Value = SpriteRect.border;
        }
    }

    [Serializable]
    internal struct OutlineData
    {
        public Vector2[] Vertices;
    }

    internal class SVGDataProviderBase
    {
        private SVGImporter m_Importer;

        public SVGDataProviderBase(SVGImporter importer)
        {
            m_Importer = importer;
        }

        public SVGSpriteData GetSVGSpriteData()
        {
            return m_Importer.GetSVGSpriteData();
        }

        public SVGImporter GetImporter()
        {
            return m_Importer;
        }

        public Sprite GetSprite()
        {
            var sprite = GetImporter().GetImportingSprite();
            if (sprite == null)
                sprite = SVGImporter.GetImportedSprite(GetImporter().assetPath);
            return sprite;
        }

        public Texture2D GetTexture2D()
        {
            var tex = GetImporter().GetImportingTexture2D();
            if (tex == null)
                tex = SVGImporter.GetImportedTexture2D(GetImporter().assetPath);
            return tex;
        }

        public Vector2 GetTextureSize()
        {
            int targetWidth;
            int targetHeight;
            GetImporter().TextureSizeForSpriteEditor(GetSprite(), out targetWidth, out targetHeight);
            return new Vector2(targetWidth, targetHeight);
        }
    }

    internal class SVGTextureDataProvider : SVGDataProviderBase, ITextureDataProvider
    {
        private float m_TextureScale = 1.0f;

        public SVGTextureDataProvider(SVGImporter importer) : base(importer)
        { }

        public Texture2D texture
        {
            get
            {
                if (GetImporter().SvgType == SVGType.TexturedSprite)
                {
                    return GetTexture2D();
                }

                // For vector sprites, the "previewTexture" property should be used, but since it was only introduced in 2018.2,
                // we fallback to the "texture" property, which works fine for textureless SVG sprites.
                // SVG sprite with textures/gradients will be displayed wrong in 2018.1.

#if UNITY_2018_2_OR_NEWER
                return null;
#else
                return previewTexture;
#endif
            }
        }

        private Texture2D m_PreviewTexture;
        public Texture2D previewTexture
        {
            get
            {
                if (GetImporter().SvgType == SVGType.TexturedSprite)
                {
                    return texture;
                }

                if (m_PreviewTexture == null)
                {
                    var sprite = GetSprite();
                    if (sprite == null)
                        return null;

                    var size = ((Vector2)sprite.bounds.size) * sprite.pixelsPerUnit;

                    const float kMinTextureSize = 2048.0f;
                    if (size.x < kMinTextureSize && size.y < kMinTextureSize)
                    {
                        var maxSize = Math.Max(size.x, size.y);
                        m_TextureScale = kMinTextureSize / maxSize;
                    }

                    var mat = SVGImporter.CreateSVGSpriteMaterial(sprite);
                    m_PreviewTexture = VectorUtils.RenderSpriteToTexture2D(sprite, (int)(size.x * m_TextureScale), (int)(size.y * m_TextureScale), mat, 4);
                    Material.DestroyImmediate(mat);
                }
                return m_PreviewTexture;
            }
        }

        public void GetTextureActualWidthAndHeight(out int width, out int height)
        {
            width = 0;
            height = 0;

            if (GetImporter().SvgType == SVGType.VectorSprite || GetImporter().SvgType == SVGType.UISVGImage)
            {
                GetImporter().TextureSizeForSpriteEditor(GetSprite(), out width, out height);
            }
            else if (GetImporter().SvgType == SVGType.TexturedSprite)
            {
                var tex = GetTexture2D();
                width = tex.width;
                height = tex.height;
            }
        }

        private Texture2D m_ReadableTexture;
        public Texture2D GetReadableTexture2D()
        {
            if (m_ReadableTexture == null)
            {
                if (GetImporter().SvgType == SVGType.VectorSprite)
                {
                    var sprite = GetSprite();
                    var size = ((Vector2)sprite.bounds.size) * sprite.pixelsPerUnit;
                    m_ReadableTexture = VectorUtils.RenderSpriteToTexture2D(sprite, (int)size.x, (int)size.y, SVGImporter.CreateSVGSpriteMaterial(sprite), 4);
                }
                else
                {
                    return GetTexture2D();
                }
            }
            return m_ReadableTexture;
        }
    }

    internal class SVGPhysicsOutlineDataProvider : SVGDataProviderBase, ISpritePhysicsOutlineDataProvider
    {
        public SVGPhysicsOutlineDataProvider(SVGImporter importer) : base(importer)
        { }

        List<Vector2[]> ISpritePhysicsOutlineDataProvider.GetOutlines(GUID guid)
        {
            if (GetSVGSpriteData().PhysicsOutlines.Count == 0)
            {
                // If no physics outline was set in the Sprite Editor, show the sprite's physics shape directly (if any)
                var sprite = GetSprite();
                if (sprite == null)
                    return null;

                var importer = GetImporter();
                int width;
                int height;
                importer.TextureSizeForSpriteEditor(sprite, out width, out height);
                var size = new Vector2(width, height);
                var offset = new Vector2(-width/2.0f, -height/2.0f);

                var storedShapes = new List<Vector2[]>(sprite.GetPhysicsShapeCount());
                var shape = new List<Vector2>();
                for (int i = 0; i < sprite.GetPhysicsShapeCount(); ++i)
                {
                    shape.Clear();
                    sprite.GetPhysicsShape(i, shape);
                    var bounds = VectorUtils.Bounds(shape);
                    for (int j = 0; j < shape.Count; ++j)
                    {
                        var p = shape[j];
                        p -= bounds.min;
                        p /= bounds.size;
                        p *= size;
                        p += offset;
                        shape[j] = p;
                    }
                    storedShapes.Add(shape.ToArray());
                }

                return storedShapes;
            }
            return GetSVGSpriteData().PhysicsOutlines.Select(x => x.Vertices.ToArray()).ToList();
        }

        void ISpritePhysicsOutlineDataProvider.SetOutlines(GUID guid, List<Vector2[]> data)
        {
            GetSVGSpriteData().PhysicsOutlines = data.Select(x => new OutlineData() { Vertices = x }).ToList();
        }

        float ISpritePhysicsOutlineDataProvider.GetTessellationDetail(GUID guid)
        {
            return GetSVGSpriteData().TessellationDetail;
        }

        void ISpritePhysicsOutlineDataProvider.SetTessellationDetail(GUID guid, float value)
        {
            GetSVGSpriteData().TessellationDetail = value;
        }
    }
}