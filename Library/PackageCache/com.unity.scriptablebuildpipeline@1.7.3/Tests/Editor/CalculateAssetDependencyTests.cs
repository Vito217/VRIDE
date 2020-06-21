using NUnit.Framework;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEditor;
using UnityEditor.Build.Content;
using UnityEditor.Build.Pipeline;
using UnityEditor.Build.Pipeline.Injector;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Tasks;
using UnityEditor.Build.Pipeline.Utilities;
using UnityEditor.U2D;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.U2D;

namespace UnityEditor.Build.Pipeline.Tests
{
    public class CalculateAssetDependencyTests
    {
        const string kTestAssetFolder = "Assets/TestAssets";
        const string kSourceTestAssetFolder = "Packages/com.unity.scriptablebuildpipeline/Tests/Editor/TestAssets";

        SpritePackerMode m_PrevMode;

        [SetUp]
        public void Setup()
        {
            m_PrevMode = EditorSettings.spritePackerMode;
            Directory.CreateDirectory(kTestAssetFolder);
        }

        [OneTimeTearDown]
        public void OneTimeTeardown()
        {
            AssetDatabase.DeleteAsset(kTestAssetFolder);
            EditorSettings.spritePackerMode = m_PrevMode;
        }

        CalculateAssetDependencyData.TaskInput CreateDefaultInput()
        {
            CalculateAssetDependencyData.TaskInput input = new CalculateAssetDependencyData.TaskInput();
            input.Target = EditorUserBuildSettings.activeBuildTarget;
            return input;
        }

        // Create a prefab and writes it to the specified path. The target file will have 2 objects in it: the GameObject and the Transform
        GUID CreateGameObject(string assetPath, string name="go")
        {
            GameObject go = new GameObject(name);
            PrefabUtility.SaveAsPrefabAsset(go, assetPath);
            UnityEngine.Object.DestroyImmediate(go, false);
            string guidString = AssetDatabase.AssetPathToGUID(assetPath);
            GUID.TryParse(guidString, out GUID guid);
            return guid;
        }

        [Test]
        public void WhenAssetHasNoDependencies()
        {
            // Create an asset
            string assetPath = Path.Combine(kTestAssetFolder, "myPrefab.prefab");
            GUID guid = CreateGameObject(assetPath);

            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { guid };

            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            Assert.AreEqual(1, output.AssetResults.Length);
            Assert.AreEqual(guid, output.AssetResults[0].asset);
            Assert.AreEqual(2, output.AssetResults[0].assetInfo.includedObjects.Count); // GameObject and Transform
            Assert.AreEqual(0, output.AssetResults[0].assetInfo.referencedObjects.Count);
            Assert.IsNull(output.AssetResults[0].spriteData);
            Assert.IsNull(output.AssetResults[0].extendedData);
        }

        [Test]
        public void WhenAssetDoesNotExist_AssetResultIsEmpty()
        {
            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { GUID.Generate() };

            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            Assert.IsNull(output.AssetResults[0].extendedData);
            Assert.AreEqual(0, output.AssetResults[0].assetInfo.includedObjects.Count);
        }

        [Test]
        public void WhenSomeAssetDataIsCached_CachedVersionIsUsed()
        {
            const int kCachedCount = 5;
            // Create 10 assets, import half of them, 
            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            string assetPath = Path.Combine(kTestAssetFolder, "myPrefab.prefab");
            List<GUID> allGUIDs = new List<GUID>();
            List<GUID> cachedGUIDs = new List<GUID>();
            for (int i = 0; i < kCachedCount;i++)
            {
                GUID cachedGUID = CreateGameObject(Path.Combine(kTestAssetFolder, $"myPrefab{i*2}.prefab"), $"go{i * 2}");
                cachedGUIDs.Add(cachedGUID);
                allGUIDs.Add(cachedGUID);
                allGUIDs.Add(CreateGameObject(Path.Combine(kTestAssetFolder, $"myPrefab{i * 2 + 1}.prefab"), $"go{i * 2 + 1}"));
            }
            
            using (BuildCache cache = new BuildCache())
            {
                input.BuildCache = cache;
                input.Assets = cachedGUIDs;
                CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);
                cache.SyncPendingSaves();
                input.Assets = allGUIDs;
                CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output2);

                Assert.AreEqual(output.AssetResults[0].assetInfo.includedObjects.Count, output2.AssetResults[0].assetInfo.includedObjects.Count); // GameObject and Transform
                Assert.AreEqual(0, output.CachedAssetCount);
                Assert.AreEqual(kCachedCount, output2.CachedAssetCount);

                for(int i = 0; i < kCachedCount; i++)
                {
                    bool seqEqual = Enumerable.SequenceEqual(output.AssetResults[i].assetInfo.includedObjects, output2.AssetResults[i * 2].assetInfo.includedObjects);
                    Assert.IsTrue(seqEqual);
                }
            }
        }

        // Embedding this shader in code and only creating it when the test actually runs so it doesn't exist outside tests.
        string kTestShader = @"Shader ""Custom / NewSurfaceShader""
{
                Properties
    {
                    _Color(""Color"", Color) = (1, 1, 1, 1)
        _MainTex(""Albedo (RGB)"", 2D) = ""white"" {
                    }
                    _Glossiness(""Smoothness"", Range(0, 1)) = 0.5
        _Metallic(""Metallic"", Range(0, 1)) = 0.0
    }
                SubShader
    {
                    Tags { ""RenderType"" = ""Opaque"" }
                    LOD 200

        CGPROGRAM
#pragma surface surf Standard fullforwardshadows
#pragma target 3.0

        sampler2D _MainTex;

        struct Input
        {
            float2 uv_MainTex;
        };

        half _Glossiness;
        half _Metallic;
        fixed4 _Color;

        UNITY_INSTANCING_BUFFER_START(Props)
        UNITY_INSTANCING_BUFFER_END(Props)

        void surf(Input IN, inout SurfaceOutputStandard o)
        {
            // Albedo comes from a texture tinted by color
            fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
            o.Albedo = c.rgb;
            // Metallic and smoothness come from slider variables
            o.Metallic = _Metallic;
            o.Smoothness = _Glossiness;
            o.Alpha = c.a;
        }
        ENDCG
    }
    FallBack ""Diffuse""
}
";


        private void CreateTestShader(string path)
        {
            if (!File.Exists(path))
            {
                File.WriteAllText(path, kTestShader);
                AssetDatabase.Refresh();
            }
        }

        [Test]
        public void WhenObjectInfluencesReferencedObjectBuildTags_BuildUsageTagsAreAdded()
        {
            string testShaderPath = Path.Combine(kTestAssetFolder, "TestShader.shader");
            CreateTestShader(testShaderPath);
            Shader shader = AssetDatabase.LoadAssetAtPath<Shader>(testShaderPath);
            string shaderGUIDString = AssetDatabase.AssetPathToGUID(testShaderPath);
            GUID.TryParse(shaderGUIDString, out GUID shaderGUID);

            // Create a material that points to the test shader asset
            Material mat = new Material(shader);
            string matPath = Path.Combine(kTestAssetFolder, "testmat.mat");
            AssetDatabase.CreateAsset(mat, matPath);
            string guidMatString = AssetDatabase.AssetPathToGUID(matPath);
            GUID.TryParse(guidMatString, out GUID matGUID);

            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { matGUID };
            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            // this define should get added to the shader build usage tags
            mat.shaderKeywords = new string[] { "TEST_DEFINE" };

            CalculateAssetDependencyData.TaskInput input2 = CreateDefaultInput();
            input2.Assets = new List<GUID>() { matGUID };
            CalculateAssetDependencyData.RunInternal(input2, out CalculateAssetDependencyData.TaskOutput output2);

            var ids = output2.AssetResults[0].usageTags.GetObjectIdentifiers();
            Assert.IsTrue(ids.Count((x) => x.guid == shaderGUID) == 1, "Shader is not in build usage tags");
            Assert.AreNotEqual(output.AssetResults[0].usageTags.GetHashCode(), output2.AssetResults[0].usageTags.GetHashCode(),"Build usage tags were not affected by material keywords");
        }

        static object [] SpriteTestCases =
        {
#if UNITY_2020_1_OR_NEWER
            new object[] { SpritePackerMode.Disabled, "", false, false },
            new object[] { SpritePackerMode.BuildTimeOnlyAtlas, "", true, true },
            new object[] { SpritePackerMode.BuildTimeOnlyAtlas, "", false, false },
#else
            new object[] { SpritePackerMode.BuildTimeOnly, "SomeTag", true, true },
            new object[] { SpritePackerMode.BuildTimeOnly, "", true, false },
            new object[] { SpritePackerMode.AlwaysOn, "SomeTag", true, true },
            new object[] { SpritePackerMode.AlwaysOn, "", true, false },
            new object[] { SpritePackerMode.Disabled, "", true, false },
            new object[] { SpritePackerMode.BuildTimeOnlyAtlas, "", true, true },
            new object[] { SpritePackerMode.AlwaysOnAtlas, "", true, true },
            new object[] { SpritePackerMode.AlwaysOnAtlas, "", false, false }
#endif
        };


        [TestCaseSource("SpriteTestCases")]
        [Test]
        public void WhenSpriteWithAtlas_SpriteImportDataCreated(SpritePackerMode spriteMode, string spritePackingTag, bool hasReferencingSpriteAtlas, bool expectedPacked)
        { 
            string sourceTexture = Path.Combine(kSourceTestAssetFolder, "SpriteTexture32x32.png");
            string destTexture = Path.Combine(kTestAssetFolder, "SpriteTexture32x32.png");
            AssetDatabase.CopyAsset(sourceTexture, destTexture);
            TextureImporter importer = AssetImporter.GetAtPath(destTexture) as TextureImporter;

            importer.spritePackingTag = spritePackingTag;
            importer.SaveAndReimport();

            if (hasReferencingSpriteAtlas)
            {
                var sa = new SpriteAtlas();
                var targetObjects = new UnityEngine.Object[] { AssetDatabase.LoadAssetAtPath<Texture>(destTexture) };
                sa.Add(targetObjects);
                string saPath = Path.Combine(kTestAssetFolder, "sa.spriteAtlas");
                AssetDatabase.CreateAsset(sa, saPath);
                AssetDatabase.Refresh();
            }

            GUID.TryParse(AssetDatabase.AssetPathToGUID(destTexture), out GUID spriteGUID);

            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            EditorSettings.spritePackerMode = spriteMode;
            SpriteAtlasUtility.PackAllAtlases(input.Target);
            input.Assets = new List<GUID>() { spriteGUID };
            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            Assert.AreEqual(expectedPacked, output.AssetResults[0].spriteData.PackedSprite);
        }

        class NullLoadRepresentationFake : CalculateAssetDependencyHooks
        {
            public override UnityEngine.Object[] LoadAllAssetRepresentationsAtPath(string assetPath) { return new UnityEngine.Object[] { null }; }
        }

        [Test]
        public void WhenAssetHasANullRepresentation_LogsWarning()
        {
            // Create an asset
            string assetPath = Path.Combine(kTestAssetFolder, "myPrefab.prefab");
            GUID guid = CreateGameObject(assetPath);

            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { guid };
            input.EngineHooks = new NullLoadRepresentationFake();

            LogAssert.Expect(LogType.Warning, new Regex(".+It will not be included in the build"));

            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            Assert.AreEqual(guid, output.AssetResults[0].asset);
            Assert.AreEqual(2, output.AssetResults[0].assetInfo.includedObjects.Count); // GameObject and Transform
            Assert.AreEqual(0, output.AssetResults[0].assetInfo.referencedObjects.Count);
        }

        [Test]
        public void WhenAssetHasMultipleRepresentations_ExtendedDataContainsAllButMainAsset()
        {
            const int kExtraRepresentations = 2;
            string assetPath = Path.Combine(kTestAssetFolder, "myPrefab.asset");
            Material mat = new Material(Shader.Find("Transparent/Diffuse"));
            AssetDatabase.CreateAsset(mat, assetPath);

            for (int i = 0; i < kExtraRepresentations; i++)
                AssetDatabase.AddObjectToAsset(new Material(Shader.Find("Transparent/Diffuse")), assetPath);
            
            AssetDatabase.SaveAssets();
            
            GUID guid = new GUID(AssetDatabase.AssetPathToGUID(assetPath));
            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { guid };

            CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);

            ObjectIdentifier []allObjIDs = ContentBuildInterface.GetPlayerObjectIdentifiersInAsset(guid, EditorUserBuildSettings.activeBuildTarget);
            HashSet<ObjectIdentifier> expectedReps = new HashSet<ObjectIdentifier>();
            for (int i = 1; i < allObjIDs.Length; i++)
                expectedReps.Add(allObjIDs[i]);

            Assert.AreEqual(kExtraRepresentations, output.AssetResults[0].extendedData.Representations.Count);
            Assert.AreEqual(kExtraRepresentations, expectedReps.Count);
            foreach (var id in output.AssetResults[0].extendedData.Representations)
                Assert.IsTrue(expectedReps.Contains(id));
        }

        class TestProgressTracker : IProgressTracker
        {
            int count = 0;
            public int TaskCount { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }

            public float Progress => throw new NotImplementedException();

            public bool UpdateInfo(string taskInfo)
            {
                return count++ > 0;
            }

            public bool UpdateTask(string taskTitle) { throw new NotImplementedException(); }
        }

        [Test]
        public void WhenCanceledThroughProgressTracker_ReturnsCanceled()
        {
            string assetPath1 = Path.Combine(kTestAssetFolder, "myPrefab1.prefab");
            string assetPath2 = Path.Combine(kTestAssetFolder, "myPrefab2.prefab");

            CalculateAssetDependencyData.TaskInput input = CreateDefaultInput();
            input.Assets = new List<GUID>() { CreateGameObject(assetPath1), CreateGameObject(assetPath2) };
            input.ProgressTracker = new TestProgressTracker();
            ReturnCode code = CalculateAssetDependencyData.RunInternal(input, out CalculateAssetDependencyData.TaskOutput output);
            Assert.AreEqual(null, output.AssetResults[1].assetInfo);
            Assert.AreEqual(ReturnCode.Canceled, code);
        }
    }
}