#if UNITY_2019_3_OR_NEWER
using NUnit.Framework;
using System.Collections.Generic;
using System.Reflection;
using UnityEditor.Build.Content;
using UnityEditor.Build.Pipeline.Injector;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Tasks;
using UnityEditor.Build.Pipeline.Utilities;
using UnityEditor.Build.Player;
using UnityEngine;

namespace UnityEditor.Build.Pipeline.Tests
{
    public class CalculateCustomDependencyTests
    {
        class TestParams : IBuildParameters
        {
            // Optional Inputs
            public BuildTarget Target { get => BuildTarget.NoTarget; set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public TypeDB ScriptInfo { get => null; set => throw new System.NotImplementedException("Should never be called by build task!"); }

            #region UNUSED
            public BuildTargetGroup Group { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public ContentBuildFlags ContentBuildFlags { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public ScriptCompilationOptions ScriptOptions { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public string TempOutputFolder { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public bool UseCache { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public string CacheServerHost { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }
            public int CacheServerPort { get => throw new System.NotImplementedException("Should never be called by build task!"); set => throw new System.NotImplementedException("Should never be called by build task!"); }

            public UnityEngine.BuildCompression GetCompressionForIdentifier(string identifier)
            {
                throw new System.NotImplementedException("Should never be called by build task!");
            }

            public BuildSettings GetContentBuildSettings()
            {
                throw new System.NotImplementedException("Should never be called by build task!");
            }

            public string GetOutputFilePathForIdentifier(string identifier)
            {
                throw new System.NotImplementedException("Should never be called by build task!");
            }

            public ScriptCompilationSettings GetScriptCompilationSettings()
            {
                throw new System.NotImplementedException("Should never be called by build task!");
            }
            #endregion
        }

        class TestContent : IBundleBuildContent
        {
            // Inputs
            public List<CustomContent> CustomAssets { get; set; }

            // Outputs
            public Dictionary<string, List<GUID>> BundleLayout { get; private set; }
            public Dictionary<GUID, string> Addresses { get; private set; }

            public TestContent(List<CustomContent> customAssets)
            {
                CustomAssets = customAssets;
                BundleLayout = new Dictionary<string, List<GUID>>();
                Addresses = new Dictionary<GUID, string>();
            }

            #region UNUSED
            public Dictionary<string, List<ResourceFile>> AdditionalFiles { get => throw new System.NotImplementedException("Should never be called by build task!"); }

            public List<GUID> Assets { get => throw new System.NotImplementedException("Should never be called by build task!"); }

            public List<GUID> Scenes { get => throw new System.NotImplementedException("Should never be called by build task!"); }
            #endregion
        }

        class TestDependencyData : IDependencyData
        {
            // Input / Output
            public Dictionary<GUID, AssetLoadInfo> AssetInfo { get; set; }

            // Optional Inputs
            public BuildUsageTagGlobal GlobalUsage { get; private set; }
            public Dictionary<GUID, SceneDependencyInfo> SceneInfo { get; private set; }
            public BuildUsageCache DependencyUsageCache => null;

            // Outputs
            public Dictionary<GUID, BuildUsageTagSet> AssetUsage { get; internal set; }

            public TestDependencyData(Dictionary<GUID, AssetLoadInfo> assetInfo)
            {
                AssetInfo = assetInfo;
                GlobalUsage = new BuildUsageTagGlobal();
                SceneInfo = new Dictionary<GUID, SceneDependencyInfo>();
                AssetUsage = new Dictionary<GUID, BuildUsageTagSet>();
            }

            #region UNUSED
            public Dictionary<GUID, BuildUsageTagSet> SceneUsage => throw new System.NotImplementedException("Should never be called by build task!");
            #endregion
        }

        static CalculateCustomDependencyData CreateDefaultBuildTask(List<CustomContent> customAssets, Dictionary<GUID, AssetLoadInfo> assetInfo = null)
        {
            var task = new CalculateCustomDependencyData();
            var testParams = new TestParams();
            var testContent = new TestContent(customAssets);
            if (assetInfo == null)
                assetInfo = new Dictionary<GUID, AssetLoadInfo>();
            var testData = new TestDependencyData(assetInfo);
            IBuildContext context = new BuildContext(testParams, testContent, testData);
            ContextInjector.Inject(context, task);
            return task;
        }

        static void ExtractTestData(IBuildTask task, out CustomAssets customAssets, out TestContent content, out TestDependencyData dependencyData)
        {
            IBuildContext context = new BuildContext();
            ContextInjector.Extract(context, task);
            customAssets = (CustomAssets)context.GetContextObject<ICustomAssets>();
            content = (TestContent)context.GetContextObject<IBundleBuildContent>();
            dependencyData = (TestDependencyData)context.GetContextObject<IDependencyData>();
        }

        static ObjectIdentifier MakeObjectId(string guid, long localIdentifierInFile, FileType fileType, string filePath)
        {
            var objectId = new ObjectIdentifier();
            var boxed = (object)objectId;
            var type = typeof(ObjectIdentifier);
            type.GetField("m_GUID", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(boxed, new GUID(guid));
            type.GetField("m_LocalIdentifierInFile", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(boxed, localIdentifierInFile);
            type.GetField("m_FileType", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(boxed, fileType);
            type.GetField("m_FilePath", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(boxed, filePath);
            return (ObjectIdentifier)boxed;
        }

        [Test]
        public void CreateAssetEntryForObjectIdentifiers_ThrowsExceptionOnAssetGUIDCollision()
        {
            var address = "CustomAssetAddress";
            var assetInfo = new Dictionary<GUID, AssetLoadInfo>();
            assetInfo.Add(HashingMethods.Calculate(address).ToGUID(), new AssetLoadInfo());

            var customContent = new List<CustomContent>
            {
                new CustomContent
                {
                    Asset = new GUID(),
                    Processor = (guid, task) =>
                    {
                        var ex = Assert.Throws<System.ArgumentException>(() => task.CreateAssetEntryForObjectIdentifiers(null, null, null, address, null));
                        var expected = string.Format("Custom Asset '{0}' already exists. Building duplicate asset entries is not supported.", address);
                        Assert.That(ex.Message, Is.EqualTo(expected));
                    }
                }
            };

            var buildTask = CreateDefaultBuildTask(customContent, assetInfo);
            buildTask.Run();
        }

        [Test]
        public void GetObjectIdentifiersAndTypesForSerializedFile_ReturnsAllObjectIdentifiersAndTypes()
        {
            var assetPath = "temp/test_serialized_file.asset";
            UnityEditorInternal.InternalEditorUtility.SaveToSerializedFileAndForget(new[] { Texture2D.whiteTexture, Texture2D.redTexture }, assetPath, false);

            var customContent = new List<CustomContent>
            {
                new CustomContent
                {
                    Asset = new GUID(),
                    Processor = (guid, task) =>
                    {
                        task.GetObjectIdentifiersAndTypesForSerializedFile(assetPath, out var objectIdentifiers, out var types);
                        Assert.AreEqual(2, objectIdentifiers.Length);
                        Assert.AreEqual(MakeObjectId("00000000000000000000000000000000", 1, FileType.NonAssetType, assetPath), objectIdentifiers[0]);
                        Assert.AreEqual(MakeObjectId("00000000000000000000000000000000", 2, FileType.NonAssetType, assetPath), objectIdentifiers[1]);

                        Assert.AreEqual(2, types.Length);
                        Assert.AreEqual(typeof(Texture2D), types[0]);
                        Assert.AreEqual(typeof(Texture2D), types[1]);
                    }
                }
            };

            var buildTask = CreateDefaultBuildTask(customContent);
            buildTask.Run();
        }

        [Test]
        public void CreateAssetEntryForObjectIdentifiers_AddsNewBundleAndAssetDataForCustomAsset()
        {
            var assetPath = "temp/test_serialized_file.asset";
            var bundleName = "CustomAssetBundle";
            var address = "CustomAssetAddress";
            var assetGuid = HashingMethods.Calculate(address).ToGUID();
            UnityEditorInternal.InternalEditorUtility.SaveToSerializedFileAndForget(new[] { Texture2D.whiteTexture, Texture2D.redTexture }, assetPath, false);

            var customContent = new List<CustomContent>
            {
                new CustomContent
                {
                    Asset = new GUID(),
                    Processor = (guid, task) =>
                    {
                        task.GetObjectIdentifiersAndTypesForSerializedFile(assetPath, out var includedObjects, out var types);
                        task.CreateAssetEntryForObjectIdentifiers(includedObjects, assetPath, bundleName, address, types[0]);
                    }
                }
            };

            var buildTask = CreateDefaultBuildTask(customContent);
            buildTask.Run();

            ExtractTestData(buildTask, out var customAssets, out var content, out var dependencyData);

            // Ensure the bundle name was added, and the custom asset guid was added to that bundle
            Assert.IsTrue(content.BundleLayout.ContainsKey(bundleName));
            CollectionAssert.Contains(content.BundleLayout[bundleName], assetGuid);

            // Ensure the custom address was added
            Assert.IsTrue(content.Addresses.ContainsKey(assetGuid));
            Assert.AreEqual(address, content.Addresses[assetGuid]);

            // Ensure AssetInfo contains the calculated includes and references for the custom asset
            Assert.IsTrue(dependencyData.AssetInfo.ContainsKey(assetGuid));
            var loadInfo = dependencyData.AssetInfo[assetGuid];
            Assert.AreEqual(address, loadInfo.address);
            Assert.AreEqual(assetGuid, loadInfo.asset);
            Assert.AreEqual(2, loadInfo.includedObjects.Count);
            Assert.AreEqual(0, loadInfo.referencedObjects.Count);

            // Ensure the usage tags were added
            Assert.IsTrue(dependencyData.AssetUsage.ContainsKey(assetGuid));
            Assert.IsNotNull(dependencyData.AssetUsage[assetGuid]);

            // Ensure the custom asset was registered in the customAssets list
            CollectionAssert.Contains(customAssets.Assets, assetGuid);
        }
    }
}
#endif