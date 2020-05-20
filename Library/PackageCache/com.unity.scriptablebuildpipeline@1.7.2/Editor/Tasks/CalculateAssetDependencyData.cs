using System.Collections.Generic;
using System.Linq;
using UnityEditor.Build.Content;
using UnityEditor.Build.Pipeline.Injector;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Utilities;
using UnityEditor.Build.Player;

namespace UnityEditor.Build.Pipeline.Tasks
{
    internal class CalculateAssetDependencyHooks
    {
        public virtual UnityEngine.Object[] LoadAllAssetRepresentationsAtPath(string assetPath)
        {
            return AssetDatabase.LoadAllAssetRepresentationsAtPath(assetPath);
        }
    }

    public class CalculateAssetDependencyData : IBuildTask
    {

        internal const int kVersion = 3;
        /// <inheritdoc />
        public int Version { get { return kVersion; } }

#pragma warning disable 649
        [InjectContext(ContextUsage.In)]
        IBuildParameters m_Parameters;

        [InjectContext(ContextUsage.In)]
        IBuildContent m_Content;

        [InjectContext]
        IDependencyData m_DependencyData;

        [InjectContext(ContextUsage.InOut, true)]
        IBuildSpriteData m_SpriteData;

        [InjectContext(ContextUsage.InOut, true)]
        IBuildExtendedAssetData m_ExtendedAssetData;

        [InjectContext(ContextUsage.In, true)]
        IProgressTracker m_Tracker;

        [InjectContext(ContextUsage.In, true)]
        IBuildCache m_Cache;
#pragma warning restore 649

        internal struct TaskInput
        {
            public IBuildCache BuildCache;
            public BuildTarget Target;
            public TypeDB TypeDB;
            public List<GUID> Assets;
            public IProgressTracker ProgressTracker;
            public BuildUsageTagGlobal GlobalUsage;
            public BuildUsageCache DependencyUsageCache;
            public CalculateAssetDependencyHooks EngineHooks;
        }

        internal struct AssetOutput
        {
            public GUID asset;
            public AssetLoadInfo assetInfo;
            public BuildUsageTagSet usageTags;
            public SpriteImporterData spriteData;
            public ExtendedAssetData extendedData;
        }

        internal struct TaskOutput
        {
            public AssetOutput [] AssetResults;
            public int CachedAssetCount;
        }

        static CachedInfo GetCachedInfo(IBuildCache cache, GUID asset, AssetLoadInfo assetInfo, BuildUsageTagSet usageTags, SpriteImporterData importerData, ExtendedAssetData assetData)
        {
            var info = new CachedInfo();
            info.Asset = cache.GetCacheEntry(asset, kVersion);

            var dependencies = new HashSet<CacheEntry>();
            foreach (var reference in assetInfo.referencedObjects)
                dependencies.Add(cache.GetCacheEntry(reference));
            info.Dependencies = dependencies.ToArray();

            info.Data = new object[] { assetInfo, usageTags, importerData, assetData };

            return info;
        }

        /// <inheritdoc />
        public ReturnCode Run()
        {
            TaskInput input = new TaskInput();
            input.Target = m_Parameters.Target;
            input.TypeDB = m_Parameters.ScriptInfo;
            input.BuildCache = m_Parameters.UseCache ? m_Cache : null;
            input.Assets = m_Content.Assets;
            input.ProgressTracker = m_Tracker;
            input.DependencyUsageCache = m_DependencyData.DependencyUsageCache;
            input.GlobalUsage = m_DependencyData.GlobalUsage;
            foreach (SceneDependencyInfo sceneInfo in m_DependencyData.SceneInfo.Values)
                input.GlobalUsage |= sceneInfo.globalUsage;

            ReturnCode code = RunInternal(input, out TaskOutput output);
            if (code == ReturnCode.Success)
            {
                foreach (AssetOutput o in output.AssetResults)
                {
                    m_DependencyData.AssetInfo.Add(o.asset, o.assetInfo);
                    m_DependencyData.AssetUsage.Add(o.asset, o.usageTags);

                    if (o.spriteData != null)
                    {
                        if (m_SpriteData == null)
                            m_SpriteData = new BuildSpriteData();
                        m_SpriteData.ImporterData.Add(o.asset, o.spriteData);
                    }

                    if (o.extendedData != null)
                    {
                        if (m_ExtendedAssetData == null)
                            m_ExtendedAssetData = new BuildExtendedAssetData();
                        m_ExtendedAssetData.ExtendedData.Add(o.asset, o.extendedData);
                    }
                }
            }

            return code;
        }

        static internal ReturnCode RunInternal(TaskInput input, out TaskOutput output)
        {
            input.EngineHooks = input.EngineHooks != null ? input.EngineHooks : new CalculateAssetDependencyHooks();
            output = new TaskOutput();
            output.AssetResults = new AssetOutput[input.Assets.Count];

            IList<CachedInfo> cachedInfo = null;
            if (input.BuildCache != null)
            {
                IList<CacheEntry> entries = input.Assets.Select(x => input.BuildCache.GetCacheEntry(x, kVersion)).ToList();
                input.BuildCache.LoadCachedData(entries, out cachedInfo);
            }

            for (int i = 0; i < input.Assets.Count; i++)
            {
                AssetOutput assetResult = new AssetOutput();
                assetResult.asset = input.Assets[i];
                if (cachedInfo != null && cachedInfo[i] != null)
                {
                    assetResult.assetInfo = cachedInfo[i].Data[0] as AssetLoadInfo;
                    assetResult.usageTags = cachedInfo[i].Data[1] as BuildUsageTagSet;
                    assetResult.spriteData = cachedInfo[i].Data[2] as SpriteImporterData;
                    assetResult.extendedData = cachedInfo[i].Data[3] as ExtendedAssetData;
                    output.AssetResults[i] = assetResult;
                    output.CachedAssetCount++;
                    continue;
                }

                GUID asset = input.Assets[i];
                string assetPath = AssetDatabase.GUIDToAssetPath(asset.ToString());

                if (!input.ProgressTracker.UpdateInfoUnchecked(assetPath))
                    return ReturnCode.Canceled;

                assetResult.assetInfo = new AssetLoadInfo();
                assetResult.usageTags = new BuildUsageTagSet();

                assetResult.assetInfo.asset = asset;
                var includedObjects = ContentBuildInterface.GetPlayerObjectIdentifiersInAsset(asset, input.Target);
                assetResult.assetInfo.includedObjects = new List<ObjectIdentifier>(includedObjects);
                var referencedObjects = ContentBuildInterface.GetPlayerDependenciesForObjects(includedObjects, input.Target, input.TypeDB);
                assetResult.assetInfo.referencedObjects = new List<ObjectIdentifier>(referencedObjects);
                var allObjects = new List<ObjectIdentifier>(includedObjects);
                allObjects.AddRange(referencedObjects);
                ContentBuildInterface.CalculateBuildUsageTags(allObjects.ToArray(), includedObjects, input.GlobalUsage, assetResult.usageTags, input.DependencyUsageCache);

                var importer = AssetImporter.GetAtPath(assetPath) as TextureImporter;
                if (importer != null && importer.textureType == TextureImporterType.Sprite)
                {
                    assetResult.spriteData = new SpriteImporterData();
                    assetResult.spriteData.PackedSprite = false;
                    assetResult.spriteData.SourceTexture = includedObjects.First();

                    if (EditorSettings.spritePackerMode != SpritePackerMode.Disabled)
                        assetResult.spriteData.PackedSprite = referencedObjects.Length > 0;
#if !UNITY_2020_1_OR_NEWER
                    if (EditorSettings.spritePackerMode == SpritePackerMode.AlwaysOn || EditorSettings.spritePackerMode == SpritePackerMode.BuildTimeOnly)
                        assetResult.spriteData.PackedSprite = !string.IsNullOrEmpty(importer.spritePackingTag);
#endif
                }

                var representations = input.EngineHooks.LoadAllAssetRepresentationsAtPath(assetPath);
                if (!representations.IsNullOrEmpty())
                {
                    assetResult.extendedData = new ExtendedAssetData();
                    for(int j = 0; j < representations.Length; j++)
                    {
                        if (representations[j] == null)
                        {
                            BuildLogger.LogWarning($"SubAsset {j} inside {assetPath} is null. It will not be included in the build.");
                            continue;
                        }

                        if (AssetDatabase.IsMainAsset(representations[j]))
                            continue;

                        string guid;
                        long localId;
                        if (!AssetDatabase.TryGetGUIDAndLocalFileIdentifier(representations[j], out guid, out localId))
                            continue;

                        assetResult.extendedData.Representations.AddRange(includedObjects.Where(x => x.localIdentifierInFile == localId));
                    }
                }
                output.AssetResults[i] = assetResult;
            }

            if (input.BuildCache != null)
            {
                List<CachedInfo> toCache = new List<CachedInfo>();
                for (int i = 0; i < input.Assets.Count; i++)
                {
                    AssetOutput r = output.AssetResults[i];
                    if (cachedInfo[i] == null)
                        toCache.Add(GetCachedInfo(input.BuildCache, input.Assets[i], r.assetInfo, r.usageTags, r.spriteData, r.extendedData));
                }
                input.BuildCache.SaveCachedData(toCache);
            }

            return ReturnCode.Success;
        }
    }
}
