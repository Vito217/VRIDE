using System;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Experimental.U2D;
using UnityEngine.Rendering;
using UnityEngine.Scripting.APIUpdating;
using UnityEngine.U2D;
using Unity.Collections;

namespace Unity.VectorGraphics
{
    /// <summary>A UI image that holds SVG content.</summary>
    [AddComponentMenu("UI/SVG Image", 11)]
    [MovedFrom("")]
    public class SVGImage : MaskableGraphic
    {
        [SerializeField] private Sprite m_Sprite = null;

        [SerializeField] private bool m_PreserveAspect = false;

        /// <summary>If true, preserves the aspect ratio of the SVG image.</summary>
        public bool preserveAspect {
            get { return m_PreserveAspect; }
            set {
                if (m_PreserveAspect != value) {
                    m_PreserveAspect = value;
                    SetVerticesDirty();
                }
            }
        }

        /// <summary>
        /// The sprite that is used to render this image.
        /// </summary>
        /// <remarks>
        /// This returns the source Sprite of an Image. This Sprite can also be viewed and changed in the Inspector as part of an Image component. This can also be used to change the Sprite using a script.
        /// </remarks>
        public Sprite sprite
        {
            get { return m_Sprite; }
            set
            {
                if (m_Sprite !=  value)
                {
                    m_Sprite = value;
                    SetAllDirty();
                }
            }
        }

        /// <summary>
        /// The main texture of the SVG image.  This will be a white texture for textureless images.
        /// </summary>
        public override Texture mainTexture
        {
            get
            {
                if (sprite == null)
                {
                    if (material != null && material.mainTexture != null)
                    {
                        return material.mainTexture;
                    }
                    return s_WhiteTexture;
                }

                return sprite.texture != null ? sprite.texture : s_WhiteTexture;
            }
        }

        static NativeSlice<Color32> s_SpriteColor = new NativeSlice<Color32>();
        static NativeSlice<Vector2> s_TextCord2 = new NativeSlice<Vector2>();
        static UIVertex s_TempVertex = new UIVertex();

        /// <summary>Populates the mesh</summary>
        /// <param name="toFill">The vertices to fill</param>
        protected override void OnPopulateMesh(VertexHelper toFill)
        {
            if (sprite == null)
            {
                base.OnPopulateMesh(toFill);
                return;
            }

            GenerateSprite(toFill);
        }

        void GenerateSprite(VertexHelper vh)
        {
            var spriteSize = new Vector2(sprite.rect.width, sprite.rect.height);

            // Covert sprite pivot into normalized space.
            var spritePivot = sprite.pivot / spriteSize;
            var rectPivot = rectTransform.pivot;
            var drawingSize = GetDrawingDimensions(m_PreserveAspect);
            var spriteBoundSize = sprite.bounds.size;

            // Calculate the drawing offset based on the difference between the two pivots.
            var drawOffset = (rectPivot - spritePivot) * drawingSize;

            bool hasColorAttribute = sprite.HasVertexAttribute(VertexAttribute.Color);
            if (hasColorAttribute)
                s_SpriteColor = sprite.GetVertexAttribute<Color32>(VertexAttribute.Color);

            bool hasTextCord2Attribute = sprite.HasVertexAttribute(VertexAttribute.TexCoord2);
            if (hasTextCord2Attribute)
                s_TextCord2 = sprite.GetVertexAttribute<Vector2>(VertexAttribute.TexCoord2);

            var color32 = color;
            vh.Clear();

            Vector2[] vertices = sprite.vertices;
            Vector2[] uvs = sprite.uv;
            for (int i = 0; i < vertices.Length; ++i)
            {
                vh.AddVert(new Vector3((vertices[i].x / spriteBoundSize.x) * drawingSize.x - drawOffset.x, (vertices[i].y / spriteBoundSize.y) * drawingSize.y - drawOffset.y), 
                    hasColorAttribute ? color32 * s_SpriteColor[i] : color32, uvs[i]);

                // VertexHelper access to uv2 isn't great work around the API for now. Copy current vert out and then back with the proper uv2 if we have it.
                if (hasTextCord2Attribute)
                {
                    vh.PopulateUIVertex(ref s_TempVertex, vh.currentVertCount - 1);
                    s_TempVertex.uv2 = s_TextCord2[i];
                    vh.SetUIVertex(s_TempVertex, vh.currentVertCount - 1);
                }
            }

            UInt16[] triangles = sprite.triangles;
            for (int i = 0; i < triangles.Length; i += 3)
            {
                vh.AddTriangle(triangles[i + 0], triangles[i + 1], triangles[i + 2]);
            }
        }

        private Vector2 GetDrawingDimensions(bool shouldPreserveAspect)
        {
            var size = new Vector2(sprite.rect.width, sprite.rect.height);

            Rect r = GetPixelAdjustedRect();

            int spriteW = Mathf.RoundToInt(size.x);
            int spriteH = Mathf.RoundToInt(size.y);

            if (shouldPreserveAspect && size.sqrMagnitude > 0.0f)
            {
                var spriteRatio = size.x / size.y;
                var rectRatio = r.width / r.height;

                if (spriteRatio > rectRatio)
                    r.height = r.width * (1.0f / spriteRatio);
                else
                    r.width = r.height * spriteRatio;
            }

            return r.size;
        }
    }
}