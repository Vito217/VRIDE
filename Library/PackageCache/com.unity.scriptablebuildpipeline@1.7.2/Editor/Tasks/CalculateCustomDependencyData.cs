#if UNITY_2019_3_OR_NEWER
using System;
using System.Collections.Generic;
using UnityEditor.Build.Content;
using UnityEditor.Build.Pipeline.Injector;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Utilities;

namespace UnityEditor.Build.Pipeline.Tasks
{
    /// <summary>
    /// Build Task that calculates teh included objects and references objects for custom assets not tracked by the AssetDatabase.
    /// <seealso cref="IBuildTask"/>
    /// </summary>
    public class CalculateCustomDependencyData : IBuildTask
    {
        /// <inheritdoc />
        public int Version { get { return 2; } }

#pragma warning disable 649
        [InjectContext(ContextUsage.In)]
        IBuildParameters m_Parameters;

        [InjectContext(ContextUsage.InOut)]
        IBundleBuildContent m_Content;

        [InjectContext(ContextUsage.InOut)]
        IDependencyData m_DependencyData;

        [InjectContext(ContextUsage.Out, true)]
        ICustomAssets m_CustomAssets;

        [InjectContext(ContextUsage.In, true)]
        IProgressTracker m_Tracker;
#pragma warning restore 649

        BuildUsageTagGlobal m_GlobalUsage;

        /// <inheritdoc />
        public ReturnCode Run()
        {
            m_CustomAssets = new CustomAssets();
            m_GlobalUsage = m_DependencyData.GlobalUsage;
            foreach (SceneDependencyInfo sceneInfo in m_DependencyData.SceneInfo.Values)
                m_GlobalUsage |= sceneInfo.globalUsage;

            foreach (CustomContent info in m_Content.CustomAssets)
            {
                if (!m_Tracker.UpdateInfoUnchecked(info.Asset.ToString()))
                    return ReturnCode.Canceled;

                info.Processor(info.Asset, this);
            }
            return ReturnCode.Success;
        }

        /// <summary>
        /// Returns the Object Identifiers and Types in a raw Unity Serialized File. The resulting arrays will be empty if a non-serialized file path was used.
        /// </summary>
        /// <param name="path">Path to the Unity Serialized File</param>
        /// <param name="objectIdentifiers">Object Identifiers for all the objects in the serialized file</param>
        /// <param name="types">Types for all the objects in the serialized file</param>
        public void GetObjectIdentifiersAndTypesForSerializedFile(string path, out ObjectIdentifier[] objectIdentifiers, out Type[] types)
        {
            objectIdentifiers = ContentBuildInterface.GetPlayerObjectIdentifiersInSerializedFile(path, m_Parameters.Target);
            types = ContentBuildInterface.GetTypeForObjects(objectIdentifiers);
        }

        /// <summary>
        /// Adds mapping and bundle information for a custom asset that contains a set of unity objects.
        /// </summary>
        /// <param name="includedObjects">Object Identifiers that belong to this custom asset</param>
        /// <param name="path">Path on disk for this custom asset</param>
        /// <param name="bundleName">Asset Bundle name where to add this custom asset</param>
        /// <param name="address">Load address to used to load this asset from the Asset Bundle</param>
        /// <param name="mainAssetType">Type of the main object for this custom asset</param>
        public void CreateAssetEntryForObjectIdentifiers(ObjectIdentifier[] includedObjects, string path, string bundleName, string address, Type mainAssetType)
        {
            AssetLoadInfo assetInfo = new AssetLoadInfo();
            BuildUsageTagSet usageTags = new BuildUsageTagSet();

            assetInfo.asset = HashingMethods.Calculate(address).ToGUID();
            assetInfo.address = address;
            if (m_DependencyData.AssetInfo.ContainsKey(assetInfo.asset))
                throw new ArgumentException(string.Format("Custom Asset '{0}' already exists. Building duplicate asset entries is not supported.", address));

            assetInfo.includedObjects = new List<ObjectIdentifier>(includedObjects);
            var referencedObjects = ContentBuildInterface.GetPlayerDependenciesForObjects(includedObjects, m_Parameters.Target, m_Parameters.ScriptInfo);
            assetInfo.referencedObjects = new List<ObjectIdentifier>(referencedObjects);
            ContentBuildInterface.CalculateBuildUsageTags(referencedObjects, includedObjects, m_GlobalUsage, usageTags, m_DependencyData.DependencyUsageCache);

            SetOutputInformation(bundleName, assetInfo, usageTags);
        }

        void SetOutputInformation(string bundleName, AssetLoadInfo assetInfo, BuildUsageTagSet usageTags)
        {
            List<GUID> assets;
            m_Content.BundleLayout.GetOrAdd(bundleName, out assets);
            assets.Add(assetInfo.asset);

            m_Content.Addresses.Add(assetInfo.asset, assetInfo.address);
            m_DependencyData.AssetInfo.Add(assetInfo.asset, assetInfo);
            m_DependencyData.AssetUsage.Add(assetInfo.asset, usageTags);
            m_CustomAssets.Assets.Add(assetInfo.asset);
        }
    }
}
#endif