using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;
using Unity.Collections;
using UnityEngine;
using UnityEditor;
#if UNITY_2019_3_OR_NEWER
using UnityEditor.U2D;
#else
using UnityEditor.Experimental.U2D;
#endif
using UnityEngine.Experimental.U2D;

#if UNITY_2020_2_OR_NEWER
using UnityEditor.AssetImporters;
#else
using UnityEditor.Experimental.AssetImporters;
#endif

using UnityEngine.Experimental.Rendering;
using UnityEditor.U2D.Sprites;

namespace Unity.VectorGraphics.Editor
{
    /// <summary>An enum describing how SVG files are imported.</summary>
    public enum SVGType
    {
        /// <summary>The SVG file will be imported as a tessellated sprite</summary>
        VectorSprite = 0,

        /// <summary>The SVG file will be imported as a textured sprite</summary>
        TexturedSprite = 1,

        /// <summary>The SVG file will be imported as a Texture2D</summary>
        Texture2D = 2,

#if UNITY_2019_3_OR_NEWER
        /// <summary>The SVG file will be imported as a UIElements Vector Image asset (deprecated)</summary>
        [Obsolete("Use UIToolkit instead")]
        UIElement = 3,

        /// <summary>The SVG file will be imported as a UI Toolkit Vector Image asset</summary>
        UIToolkit = 3,
#endif
        /// <summary>The SVG file will be imported as a tessellated sprite, compatible with the UI canvas masking system</summary>
        UISVGImage = 4,
    }

    /// <summary>The SVG importer class.</summary>
    [Serializable]
    [ScriptedImporter(5, "svg")]
    public class SVGImporter : ScriptedImporter, ISpriteEditorDataProvider
    {
        internal static readonly string k_PackagePath = "Packages/com.unity.vectorgraphics";

        /// <summary>How the SVG file will be imported</summary>
        public SVGType SvgType {
            get { return m_SvgType; }
            set { m_SvgType = value; }
        }
        [SerializeField] private SVGType m_SvgType = SVGType.VectorSprite;

        /// <summary>For textured sprite, the mesh type</summary>
        public SpriteMeshType TexturedSpriteMeshType
        {
            get { return m_TexturedSpriteMeshType; }
            set { m_TexturedSpriteMeshType = value; }
        }
        [SerializeField] private SpriteMeshType m_TexturedSpriteMeshType = SpriteMeshType.FullRect;

        /// <summary>The number of pixels per Unity units.</summary>
        public float SvgPixelsPerUnit {
            get { return m_SvgPixelsPerUnit; }
            set { m_SvgPixelsPerUnit = value; }
        }
        [SerializeField] private float m_SvgPixelsPerUnit = 100.0f;

        /// <summary>Maximum resolution for gradient texture.</summary>
        public UInt16 GradientResolution {
            get { return m_GradientResolution; }
            set { m_GradientResolution = value; }
        }
        [SerializeField] private UInt16 m_GradientResolution = 64;

        /// <summary>The SVG sprite alignement.</summary>
        public VectorUtils.Alignment Alignment {
            get { return m_Alignment; }
            set { m_Alignment = value; }
        }
        [SerializeField] private VectorUtils.Alignment m_Alignment;

        /// <summary>The custom pivot, when alignement is "Custom".</summary>
        public Vector2 CustomPivot {
            get { return m_CustomPivot; }
            set { m_CustomPivot = value; }
        }
        [SerializeField] private Vector2 m_CustomPivot;

        /// <summary>Automaticallly generates a physics shape.</summary>
        public bool GeneratePhysicsShape {
            get { return m_GeneratePhysicsShape; }
            set { m_GeneratePhysicsShape = value; }
        }
        [SerializeField] private bool m_GeneratePhysicsShape;

        /// <summary>Viewport options to use when importing the SVG document.</summary>
        public ViewportOptions ViewportOptions {
            get { return m_ViewportOptions; }
            set { m_ViewportOptions = value; }
        }
        [SerializeField] private ViewportOptions m_ViewportOptions = ViewportOptions.DontPreserve;

         /// <summary>Preserves the viewport defined in the SVG document.</summary>
         [Obsolete("Use the ViewportOptions property instead")]
         public bool PreserveViewport {
             get { return m_PreserveViewport; }
             set { m_PreserveViewport = value; }
         }
         [SerializeField] private bool m_PreserveViewport;
 
        /// <summary>Use advanced settings.</summary>
        public bool AdvancedMode {
            get { return m_AdvancedMode; }
            set { m_AdvancedMode = value; }
        }
        [SerializeField] private bool m_AdvancedMode;

        /// <summary>The predefined resolution used, when not in advanced mode.</summary>
        public int PredefinedResolutionIndex {
            get { return m_PredefinedResolutionIndex; }
            set { m_PredefinedResolutionIndex = value; }
        }
        [SerializeField] private int m_PredefinedResolutionIndex = 1;

        /// <summary>The target resolution on which this SVG is displayed.</summary>
        public int TargetResolution {
            get { return m_TargetResolution; }
            set { m_TargetResolution = value; }
        }
        [SerializeField] private int m_TargetResolution = 1080;

        /// <summary>An additional scale factor on the target resolution.</summary>
        public float ResolutionMultiplier {
            get { return m_ResolutionMultiplier; }
            set { m_ResolutionMultiplier = value; }
        }
        [SerializeField] private float m_ResolutionMultiplier = 1.0f;

        /// <summary>The uniform step distance used for tessellation.</summary>
        public float StepDistance {
            get { return m_StepDistance; }
            set { m_StepDistance = value; }
        }
        [SerializeField] private float m_StepDistance = 10.0f;

        /// <summary>Number of samples evaluated on paths.</summary>
        public float SamplingStepDistance {
            get { return m_SamplingStepDistance; }
            set { m_SamplingStepDistance = value; }
        }
        [SerializeField] private float m_SamplingStepDistance = 100.0f;

        /// <summary>Enables the "max coord deviation" constraint.</summary>
        public bool MaxCordDeviationEnabled {
            get { return m_MaxCordDeviationEnabled; }
            set { m_MaxCordDeviationEnabled = value; }
        }
        [SerializeField] private bool m_MaxCordDeviationEnabled = false;

        /// <summary>Distance on the cord to a straight line between two points after which more tessellation will be generated.</summary>
        public float MaxCordDeviation {
            get { return m_MaxCordDeviation; }
            set { m_MaxCordDeviation = value; }
        }
        [SerializeField] private float m_MaxCordDeviation = 1.0f;

        /// <summary>Enables the "max tangent angle" constraint.</summary>
        public bool MaxTangentAngleEnabled {
            get { return m_MaxTangentAngleEnabled; }
            set { m_MaxTangentAngleEnabled = value; }
        }
        [SerializeField] private bool m_MaxTangentAngleEnabled = false;

        /// <summary>Max tangent angle (in degrees) after which more tessellation will be generated.</summary>
        public float MaxTangentAngle {
            get { return m_MaxTangentAngle; }
            set { m_MaxTangentAngle = value; }
        }
        [SerializeField] private float m_MaxTangentAngle = 5.0f;

        /// <summary>The size of the texture (only used when importing to a texture).</summary>
        public bool KeepTextureAspectRatio {
            get { return m_KeepTextureAspectRatio; }
            set { m_KeepTextureAspectRatio = value; }
        }
        [SerializeField] private bool m_KeepTextureAspectRatio = true;

        /// <summary>The size of the texture (only used when importing to a texture with "keep aspect ratio").</summary>
        public int TextureSize {
            get { return m_TextureSize; }
            set { m_TextureSize = value; }
        }
        [SerializeField] private int m_TextureSize = 256;

        /// <summary>The width of the texture (only used when importing to a texture).</summary>
        public int TextureWidth {
            get { return m_TextureWidth; }
            set { m_TextureWidth = value; }
        }
        [SerializeField] private int m_TextureWidth = 256;

        /// <summary>The height of the texture (only used when importing to a texture).</summary>
        public int TextureHeight {
            get { return m_TextureHeight; }
            set { m_TextureHeight = value; }
        }
        [SerializeField] private int m_TextureHeight = 256;

        /// <summary>The wrap mode of the texture (only used when importing to a texture).</summary>
        public TextureWrapMode WrapMode {
            get { return m_WrapMode; }
            set { m_WrapMode = value; }
        }
        [SerializeField] private TextureWrapMode m_WrapMode = TextureWrapMode.Repeat;

        /// <summary>The filter mode of the texture (only used when importing to a texture).</summary>
        public FilterMode FilterMode {
            get { return m_FilterMode; }
            set { m_FilterMode = value; }
        }
        [SerializeField] private FilterMode m_FilterMode = FilterMode.Bilinear;

        /// <summary>The number of samples per pixel (only used when importing to a texture).</summary>
        public int SampleCount
        {
            get { return m_SampleCount; }
            set { m_SampleCount = value; }
        }
        [SerializeField] private int m_SampleCount = 4;

        /// <summary>When importing to an SVGImage, preserves the aspect ratio of the generated sprite.</summary>
        public bool PreserveSVGImageAspect
        {
            get { return m_PreserveSVGImageAspect; }
            set { m_PreserveSVGImageAspect = value; }
        }
        [SerializeField] private bool m_PreserveSVGImageAspect;

        /// <summary></summary>
        public bool UseSVGPixelsPerUnit
        {
            get { return m_UseSVGPixelsPerUnit; }
            set { m_UseSVGPixelsPerUnit = value; }
        }
        [SerializeField] private bool m_UseSVGPixelsPerUnit;


        [SerializeField]
        private SVGSpriteData m_SpriteData = new SVGSpriteData();
        internal SVGSpriteData GetSVGSpriteData() { return m_SpriteData; }

        internal Sprite GetImportingSprite() { return m_ImportingSprite; }
        private Sprite m_ImportingSprite;

        internal Texture2D GetImportingTexture2D() { return m_ImportingTexture2D; }
        private Texture2D m_ImportingTexture2D;

        internal enum PredefinedResolution
        {
            Res_2160p,
            Res_1080p,
            Res_720p,
            Res_480p,
            Custom
        }

        private static float kDefaultPhysicsTessellationDetail = 0.25f;
        private static byte kDefaultSpritePhysicsAlphaTolerance = 200;

        /// <summary>Imports an SVG asset</summary>
        /// <param name="ctx">The asset import context of the scripted importer</param>
        public override void OnImportAsset(AssetImportContext ctx)
        {
            UpdateProperties();

            // We're using a hardcoded window size of 100x100. This way, using a pixels per point value of 100
            // results in a sprite of size 1 when the SVG file has a viewbox specified.
            SVGParser.SceneInfo sceneInfo;
            using (var stream = new StreamReader(ctx.assetPath))
                sceneInfo = SVGParser.ImportSVG(stream, ViewportOptions, 0, 1, 100, 100);

            if (sceneInfo.Scene == null || sceneInfo.Scene.Root == null)
                throw new Exception("Wowzers!");

            float stepDist = StepDistance;
            float samplingStepDist = SamplingStepDistance;
            float maxCord = MaxCordDeviationEnabled ? MaxCordDeviation : float.MaxValue;
            float maxTangent = MaxTangentAngleEnabled ? MaxTangentAngle : Mathf.PI * 0.5f;

            if (!AdvancedMode)
            {
                // Automatically compute sensible tessellation options from the
                // vector scene's bouding box and target resolution
                ComputeTessellationOptions(sceneInfo, TargetResolution, ResolutionMultiplier, out stepDist, out maxCord, out maxTangent);
            }

            var tessOptions = new VectorUtils.TessellationOptions();
            tessOptions.MaxCordDeviation = maxCord;
            tessOptions.MaxTanAngleDeviation = maxTangent;
            tessOptions.SamplingStepSize = 1.0f / (float)samplingStepDist;
            tessOptions.StepDistance = stepDist;

            var rect = Rect.zero;
            if (ViewportOptions == ViewportOptions.PreserveViewport)
                rect = sceneInfo.SceneViewport;

            var geometry = VectorUtils.TessellateScene(sceneInfo.Scene, tessOptions, sceneInfo.NodeOpacity);
            var name = System.IO.Path.GetFileNameWithoutExtension(ctx.assetPath);
            Sprite sprite = null;

            switch (SvgType)
            {
                case SVGType.VectorSprite:
                    sprite = BuildSpriteFromGeometry(geometry, rect);
                    GenerateSpriteAsset(ctx, sprite, name);
                    break;
                case SVGType.UISVGImage:
                    sprite = BuildSpriteFromGeometry(geometry, rect);
                    GenerateUGUISpriteAsset(ctx, sprite, name);
                    break;
                case SVGType.TexturedSprite:
                    sprite = BuildSpriteFromGeometry(geometry, rect);
                    GenerateTexturedSpriteAsset(ctx, sprite, name);
                    break;
                case SVGType.Texture2D:
                    sprite = BuildSpriteFromGeometry(geometry, rect);
                    GenerateTexture2DAsset(ctx, sprite, name);
                    break;
#if UNITY_2019_3_OR_NEWER
                case SVGType.UIToolkit:
                    GenerateVectorImageAsset(ctx, geometry, name);
                    break;
#endif
                default:
                    break;
            }
        }

        private Sprite BuildSpriteFromGeometry(List<VectorUtils.Geometry> geometry, Rect rect)
        {
            return VectorUtils.BuildSprite(geometry, rect, SvgPixelsPerUnit, Alignment, CustomPivot, GradientResolution, true);
        }

        private void UpdateProperties()
        {
            // The "PreserveViewport" property is deprecated and should be moved to the "ViewportOptions" property
            if (m_PreserveViewport)
            {
                m_ViewportOptions = ViewportOptions.PreserveViewport;
                m_PreserveViewport = false;
            }
            EditorUtility.SetDirty(this);
        }

        private void PrepareSpriteAsset(Sprite sprite, string name)
        {
            sprite.name = name + "Sprite";
            if (sprite.texture != null)
                sprite.texture.name = name + "Atlas";

            m_ImportingSprite = sprite;

            // Apply GUID from SpriteRect
#if UNITY_2018_2_OR_NEWER
            sprite.SetSpriteID(m_SpriteData.SpriteRect.spriteID);
#else
            var so = new SerializedObject(sprite);
            so.FindProperty("m_SpriteID").stringValue = m_SpriteData.SpriteRect.spriteID.ToString();
            so.ApplyModifiedPropertiesWithoutUndo();
#endif

            sprite.hideFlags = HideFlags.None;
        }

        private void GenerateSpriteAsset(AssetImportContext ctx, Sprite sprite, string name)
        {
            PrepareSpriteAsset(sprite, name);

            ctx.AddObjectToAsset("sprite", sprite);

            Material mat = MaterialForSVGSprite(sprite);

            var gameObject = new GameObject(name);
            var spriteRenderer = gameObject.AddComponent<SpriteRenderer>();
            spriteRenderer.sprite = sprite;
            spriteRenderer.material = mat;

            SetPhysicsShape(sprite);

            if (sprite.texture != null)
                ctx.AddObjectToAsset("texAtlas", sprite.texture);

            ctx.AddObjectToAsset("gameObject", gameObject);
            ctx.SetMainObject(gameObject);
        }

        private void GenerateUGUISpriteAsset(AssetImportContext ctx, Sprite sprite, string name)
        {
            PrepareSpriteAsset(sprite, name);

            ctx.AddObjectToAsset("sprite", sprite);

            Material mat = MaterialForSVGSprite(sprite);

            var gameObject = new GameObject(name);
            gameObject.AddComponent<CanvasRenderer>();
            gameObject.AddComponent<RectTransform>();

            var svgImage = gameObject.AddComponent<SVGImage>();
            svgImage.sprite = sprite;
            svgImage.material = mat;
            svgImage.preserveAspect = PreserveSVGImageAspect;

            SetPhysicsShape(sprite);

            if (sprite.texture != null)
                ctx.AddObjectToAsset("texAtlas", sprite.texture);

            ctx.AddObjectToAsset("gameObject", gameObject);
            ctx.SetMainObject(gameObject);
        }

        private void GenerateTexturedSpriteAsset(AssetImportContext ctx, Sprite sprite, string name)
        {
            if (sprite.texture != null)
                sprite.texture.name = name + "Atlas";

            var tex = BuildTexture(sprite, name);
            var rect = new Rect(0, 0, tex.width, tex.height);

            // The pivot of the orignial sprite was properly computed, just translate it to the new textured sprite
            var pivot = sprite.pivot;
            pivot /= sprite.rect.size;

            float ratio = 1.0f;
            if (m_UseSVGPixelsPerUnit)
            {
                ratio = ((float)tex.width) / sprite.rect.width;
                ratio = Mathf.Max(0.001f, ratio);
            }

            var texturedSprite = Sprite.Create(tex, rect, pivot, SvgPixelsPerUnit * ratio, 0, TexturedSpriteMeshType, m_SpriteData.SpriteRect.border);
            texturedSprite.name = name;

            m_ImportingSprite = texturedSprite;
            m_ImportingTexture2D = tex;

            // Apply GUID from SpriteRect
#if UNITY_2018_2_OR_NEWER
            texturedSprite.SetSpriteID(m_SpriteData.SpriteRect.spriteID);
#else
            var so = new SerializedObject(texturedSprite);
            so.FindProperty("m_SpriteID").stringValue = m_SpriteData.SpriteRect.spriteID.ToString();
            so.ApplyModifiedPropertiesWithoutUndo();
#endif

            texturedSprite.hideFlags = HideFlags.None;

            SetPhysicsShape(texturedSprite);

            ctx.AddObjectToAsset("sprite", texturedSprite);
            ctx.AddObjectToAsset("tex", tex);
            ctx.SetMainObject(tex);

            if (sprite.texture != null)
                GameObject.DestroyImmediate(sprite.texture);
            GameObject.DestroyImmediate(sprite);
        }

        private void GenerateTexture2DAsset(AssetImportContext ctx, Sprite sprite, string name)
        {
            var tex = BuildTexture(sprite, name);

            m_ImportingTexture2D = tex;

            ctx.AddObjectToAsset("tex", tex);
            ctx.SetMainObject(tex);

            if (sprite.texture != null)
                GameObject.DestroyImmediate(sprite.texture);
            GameObject.DestroyImmediate(sprite);
        }

#if UNITY_2019_3_OR_NEWER
        private void GenerateVectorImageAsset(AssetImportContext ctx, List<VectorUtils.Geometry> geometry, string name)
        {
            UnityEngine.Object asset;
            Texture2D texAtlas;
            Unity.VectorGraphics.InternalBridge.MakeVectorImageAsset(geometry, GradientResolution, out asset, out texAtlas);

            if (asset == null)
            {
                Debug.LogError("UIElement asset generation failed");
                return;
            }

            if (texAtlas != null)
                texAtlas.name = name + "Atlas";

            ctx.AddObjectToAsset("uiAsset", asset);
            if (texAtlas != null)
                ctx.AddObjectToAsset("tex", texAtlas);
            ctx.SetMainObject(asset);
        }
#endif

        private Texture2D BuildTexture(Sprite sprite, string name)
        {
            int textureWidth = 0;
            int textureHeight = 0;
            if (KeepTextureAspectRatio)
            {
                ComputeTextureDimensionsFromBounds(sprite, TextureSize, out textureWidth, out textureHeight);
            }
            else
            {
                textureWidth = TextureWidth;
                textureHeight = TextureHeight;
            }

            Material mat = MaterialForSVGSprite(sprite);

            // Expand edges to avoid bilinear filter "edge outlines" caused by transparent black background.
            // Not necessary when using point filtering with 1 sample.
            bool expandEdges = FilterMode != FilterMode.Point || SampleCount > 1;

            var tex = VectorUtils.RenderSpriteToTexture2D(sprite, textureWidth, textureHeight, mat, SampleCount, expandEdges);
            tex.hideFlags = HideFlags.None;
            tex.name = name;
            tex.wrapMode = WrapMode;
            tex.filterMode = FilterMode;

            return tex;
        }

        private void SetPhysicsShape(Sprite sprite)
        {
            var physicsDataProvider = (this as ISpriteEditorDataProvider).GetDataProvider<ISpritePhysicsOutlineDataProvider>();
            var outlines = physicsDataProvider.GetOutlines(m_SpriteData.SpriteRect.spriteID);
            if (outlines.Count == 0)
            {
                if (!GeneratePhysicsShape)
                    return;

                var textureDataProvider = (this as ISpriteEditorDataProvider).GetDataProvider<ITextureDataProvider>();
                var tex = textureDataProvider.GetReadableTexture2D();

                outlines = InternalEditorBridge.GenerateOutline(tex, new Rect(0,0,tex.width,tex.height), kDefaultPhysicsTessellationDetail, kDefaultSpritePhysicsAlphaTolerance, false);
                if (outlines == null || outlines.Count == 0)
                    return;
            }

            int width;
            int height;
            TextureSizeForSpriteEditor(sprite, out width, out height);

            // Offset the outline inside the sprite
            foreach (var outline in outlines)
            {
                for (int i = 0; i < outline.Length; ++i)
                {
                    var v = outline[i];
                    v.x += width / 2.0f;
                    v.y += height / 2.0f;
                    outline[i] = v;
                }
            }

            sprite.OverridePhysicsShape(outlines.Where(o => o.Length > 2).ToArray());
        }

        private Material MaterialForSVGSprite(Sprite sprite)
        {
            string path;
            if (sprite.texture != null)
                // When texture is present, use the VectorGradient shader
                path = k_PackagePath + "/Runtime/Materials/Unlit_VectorGradient";
            else
                path = k_PackagePath + "/Runtime/Materials/Unlit_Vector";

            if (SvgType == SVGType.UISVGImage)
                path += "UI";

            path += ".mat";

            return AssetDatabase.LoadAssetAtPath<Material>(path);
        }

        private void ComputeTextureDimensionsFromBounds(Sprite sprite, int textureSize, out int textureWidth, out int textureHeight)
        {
            var bounds = sprite.bounds;
            if (bounds.size.y < Mathf.Epsilon)
            {
                textureWidth = textureSize;
                textureHeight = textureSize;
                return;
            }

            float ratio = bounds.size.x / bounds.size.y;
            if (ratio >= 1.0f)
            {
                textureWidth = TextureSize;
                textureHeight = Mathf.RoundToInt((float)TextureSize / ratio);
            }
            else
            {
                textureWidth = Mathf.RoundToInt((float)TextureSize * ratio);
                textureHeight = TextureSize;
            }
        }

        private void ComputeTessellationOptions(SVGParser.SceneInfo sceneInfo, int targetResolution, float multiplier, out float stepDist, out float maxCord, out float maxTangent)
        {
            // These tessellation options were found by trial and error to find values that made
            // visual sense with a variety of SVG assets.

            // "Pixels per Unit" doesn't make sense for UI Toolkit since it will be displayed in
            // a pixels space.  We adjust the magic values below accordingly.
            #if UNITY_2019_3_OR_NEWER
            float ppu = (SvgType == SVGType.UIToolkit) ? 1.0f : SvgPixelsPerUnit;
            #else
            float ppu = SvgPixelsPerUnit;
            #endif

            var bbox = VectorUtils.ApproximateSceneNodeBounds(sceneInfo.Scene.Root);
            float maxDim = Mathf.Max(bbox.width, bbox.height) / ppu;

            // The scene ratio gives a rough estimate of coverage % of the vector scene on the screen.
            // Higher values should result in a more dense tessellation.
            float sceneRatio = maxDim / (targetResolution * multiplier);

            stepDist = float.MaxValue; // No need for uniform step distance
            #if UNITY_2019_3_OR_NEWER
            if (SvgType == SVGType.UIToolkit)
            {
                maxCord = Mathf.Max(0.01f, 2.0f * sceneRatio);
                maxTangent = Mathf.Max(0.1f, 3.0f * sceneRatio);
            }
            else
            #endif
            {
                maxCord = Mathf.Max(0.01f, 75.0f * sceneRatio);
                maxTangent = Mathf.Max(0.1f, 100.0f * sceneRatio);
            }
        }

        internal static Sprite GetImportedSprite(string assetPath)
        {
            return AssetDatabase.LoadAssetAtPath<Sprite>(assetPath);
        }

        internal static Sprite GetImportedSprite(UnityEngine.Object asset)
        {

            var sprite = asset as Sprite;
            if (sprite != null)
                return sprite;

            var go = asset as GameObject;

            // Try with SpriteRenderer
            var sr = go != null ? go.GetComponent<SpriteRenderer>() : null;
            sprite = sr != null ? sr.sprite : null;
            if (sprite != null)
                return sprite;

            // Try with VectorImage            
            var si = go != null ? go.GetComponent<SVGImage>() : null;
            sprite = si != null ? si.sprite : null;
            if (sprite != null)
                return sprite;

            return null;
        }

        internal static Texture2D GetImportedTexture2D(string assetPath)
        {
            return AssetDatabase.LoadMainAssetAtPath(assetPath) as Texture2D;
        }

        internal void TextureSizeForSpriteEditor(Sprite sprite, out int width, out int height)
        {
            var size = ((Vector2)sprite.bounds.size) * SvgPixelsPerUnit;
            width = (int)(size.x + 0.5f);
            height = (int)(size.y + 0.5f);
        }

        private static Material s_VectorMat = null;
        private static Material s_GradientMat = null;

        internal static Material CreateSVGSpriteMaterial(Sprite sprite)
        {
            if (sprite == null)
                return null;
            return CreateSVGSpriteMaterial(sprite.texture != null);
        }

        internal static Material CreateSVGSpriteMaterial(bool hasTexture)
        {
            Material mat = null;
            if (hasTexture)
            {
                if (s_GradientMat == null)
                {
                    string gradientMatPath = "Packages/com.unity.vectorgraphics/Runtime/Materials/Unlit_VectorGradient.mat";
                    s_GradientMat = AssetDatabase.LoadMainAssetAtPath(gradientMatPath) as Material;
                }
                mat = new Material(s_GradientMat);
            }
            else
            {
                if (s_VectorMat == null)
                {
                    string vectorMatPath = "Packages/com.unity.vectorgraphics/Runtime/Materials/Unlit_Vector.mat";
                    s_VectorMat = AssetDatabase.LoadMainAssetAtPath(vectorMatPath) as Material;
                }
                mat = new Material(s_VectorMat);
            }
            return mat;
        }

        // ISpriteEditorDataProvider methods

        /// <summary>Returns the sprite import mode</summary>
        /// <remarks>For SVG files, the import mode is always "single"</remarks>
        public SpriteImportMode spriteImportMode
        {
             get { return SpriteImportMode.Single; }
        }

        /// <summary>Returns pixels per unit of the imported SVG</summary>
        public float pixelsPerUnit
        {
            get { return SvgPixelsPerUnit; }
        }
        
        /// <summary>Returns imported sprite</summary>
        public UnityEngine.Object targetObject
        {
            get { return GetImportedSprite(assetPath); }
        }

        /// <summary>Returns the sprite rectangles</summary>
        /// <returns>An array of the sprite rectangles</returns>
        SpriteRect[] ISpriteEditorDataProvider.GetSpriteRects()
        {
            return new SpriteRect[] { m_SpriteData.SpriteRect };
        }

        /// <summary>Sets the sprite rectangles</summary>
        /// <param name="rects">The new sprite rectangles to use</param>
        void ISpriteEditorDataProvider.SetSpriteRects(SpriteRect[] rects)
        {
            if (rects.Length > 0)
                m_SpriteData.SpriteRect = rects[0];
        }

        /// <summary>Applies the modified SVG data</summary>
        void ISpriteEditorDataProvider.Apply()
        {
            var so = new SerializedObject(this);
            m_SpriteData.Apply(so);
            so.ApplyModifiedPropertiesWithoutUndo();
        }

        /// <summary>Initializes the sprite editor data</summary>
        void ISpriteEditorDataProvider.InitSpriteEditorDataProvider()
        {
            var so = new SerializedObject(this);
            m_SpriteData.Load(so);
        }

        /// <summary>Gets the data provider for a given type</summary>
        /// <typeparam name="T">The type of the data provider</typeparam>
        /// <returns>The data provider</returns>
        T ISpriteEditorDataProvider.GetDataProvider<T>()
        {
            if (typeof(T) == typeof(ISpritePhysicsOutlineDataProvider))
            {
                return new SVGPhysicsOutlineDataProvider(this) as T;
            }
            if (typeof(T) == typeof(ITextureDataProvider))
            {
                return new SVGTextureDataProvider(this) as T;
            }
            else if (typeof(T) == typeof(ISpriteEditorDataProvider))
            {
                return this as T;
            }
            return null;
        }

        /// <summary>Gets the data provider for a given type</summary>
        /// <param name="type">The type</param>
        /// <returns>True if a data provider is available for the type, or false otherwise</returns>
        bool ISpriteEditorDataProvider.HasDataProvider(Type type)
        {
            if (type == typeof(ISpritePhysicsOutlineDataProvider) ||
                type == typeof(ITextureDataProvider))
            {
                return true;
            }
            return false;
        }
    }
}
