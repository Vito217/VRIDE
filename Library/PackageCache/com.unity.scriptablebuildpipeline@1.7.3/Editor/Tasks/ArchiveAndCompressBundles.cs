using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using UnityEditor.Build.Content;
using UnityEditor.Build.Pipeline.Injector;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Utilities;
using UnityEngine;
using UnityEngine.Build.Pipeline;

namespace UnityEditor.Build.Pipeline.Tasks
{
#if UNITY_2018_3_OR_NEWER
    using BuildCompression = UnityEngine.BuildCompression;
#else
    using BuildCompression = UnityEditor.Build.Content.BuildCompression;
#endif

    public class ArchiveAndCompressBundles : IBuildTask
    {
        private const int kVersion = 1;
        /// <inheritdoc />
        public int Version { get { return kVersion; } }

#pragma warning disable 649
        [InjectContext(ContextUsage.In)]
        IBuildParameters m_Parameters;

        [InjectContext(ContextUsage.In)]
        IBundleWriteData m_WriteData;

#if UNITY_2019_3_OR_NEWER
        [InjectContext(ContextUsage.In)]
        IBundleBuildContent m_Content;
#endif

        [InjectContext]
        IBundleBuildResults m_Results;

        [InjectContext(ContextUsage.In, true)]
        IProgressTracker m_Tracker;

        [InjectContext(ContextUsage.In, true)]
        IBuildCache m_Cache;

        [InjectContext(ContextUsage.In, true)]
        IBuildLogger m_Log;
#pragma warning restore 649

        static internal bool m_SupportsMultiThreadedArching;
        static ArchiveAndCompressBundles()
        {
            SupportsMultiThreadedArchiving = false;
            foreach (MethodInfo info in typeof(ContentBuildInterface).GetMethods())
            {
                if (info.Name == "ArchiveAndCompress")
                {
                    foreach (var attr in info.CustomAttributes)
                    {
                        string name = attr.AttributeType.Name;
                        if (name == "ThreadSafeAttribute")
                        {
                            SupportsMultiThreadedArchiving = true;
                            break;
                        }
                    }
                }
            }
        }

        static void CopyFileWithTimestampIfDifferent(string srcPath, string destPath, IBuildLogger log)
        {
            if (srcPath == destPath)
                return;

            DateTime time = File.GetLastWriteTime(srcPath);
            DateTime destTime = File.Exists(destPath) ? File.GetLastWriteTime(destPath) : new DateTime();

            if (destTime == time)
                return;

            using (log.ScopedStep(LogLevel.Verbose, $"Copying From Cache {srcPath} -> {destPath}"))
            {
                var directory = Path.GetDirectoryName(destPath);
                Directory.CreateDirectory(directory);
                File.Copy(srcPath, destPath, true);
            }
        }

        static internal bool SupportsMultiThreadedArchiving { get; private set; }

        static CacheEntry GetCacheEntry(IBuildCache cache, string bundleName, IEnumerable<ResourceFile> resources, BuildCompression compression)
        {
            var entry = new CacheEntry();
            entry.Type = CacheEntry.EntryType.Data;
            entry.Guid = HashingMethods.Calculate("ArchiveAndCompressBundles", bundleName).ToGUID();
            List<object> toHash = new List<object> { kVersion, compression };
            foreach (var resource in resources)
            {
                toHash.Add(resource.serializedFile);
                toHash.Add(resource.fileAlias);
                toHash.Add(cache.GetCacheEntry(resource.fileName).Hash);
            }
            entry.Hash = HashingMethods.Calculate(toHash).ToHash128();
            entry.Version = kVersion;
            return entry;
        }

        static CachedInfo GetCachedInfo(IBuildCache cache, CacheEntry entry, IEnumerable<ResourceFile> resources, BundleDetails details)
        {
            var info = new CachedInfo();
            info.Asset = entry;
            info.Dependencies = new CacheEntry[0];
            info.Data = new object[] { details };
            return info;
        }

        internal static Hash128 CalculateHashVersion(Dictionary<string, ulong> fileOffsets, ResourceFile[] resourceFiles, string[] dependencies)
        {
            List<RawHash> hashes = new List<RawHash>();

            foreach (ResourceFile file in resourceFiles)
            {
                if (file.serializedFile)
                {
                    // For serialized files, we ignore the header for the hash value.
                    // This leaves us with a hash value of just the written object data.
                    using (var stream = new FileStream(file.fileName, FileMode.Open, FileAccess.Read))
                    {
                        stream.Position = (long)fileOffsets[file.fileName];
                        hashes.Add(HashingMethods.CalculateStream(stream));
                    }
                }
                else
                    hashes.Add(HashingMethods.CalculateFile(file.fileName));
            }

            return HashingMethods.Calculate(hashes, dependencies).ToHash128();
        }

        class ArchiveWorkItem
        {
            public int Index;
            public string BundleName;
            public string OutputFilePath;
            public string CachedArtifactPath;
            public ResourceFile[] ResourceFiles;
            public BuildCompression Compression;
            public BundleDetails ResultDetails;
            public Hash128 ResultHash;
        }

        internal struct TaskInput
        {
            public Dictionary<string, WriteResult> InternalFilenameToWriteResults;
#if UNITY_2019_3_OR_NEWER
            public Dictionary<string, List<ResourceFile>> InternalFilenameToAdditionalFiles;
#endif
            public Dictionary<string, string> InternalFilenameToBundleName;
            public Func<string, BuildCompression> GetCompressionForIdentifier;
            public Func<string, string> GetOutputFilePathForIdentifier;
            public IBuildCache BuildCache;
            public Dictionary<GUID, List<string>> AssetToFilesDependencies;
            public IProgressTracker ProgressTracker;
            public string TempOutputFolder;
            public bool Threaded;
            public List<string> OutCachedBundles;
            public IBuildLogger Log;
        }

        internal struct TaskOutput
        {
            public Dictionary<string, BundleDetails> BundleDetails;
        }

        /// <inheritdoc />
        public ReturnCode Run()
        {
            TaskInput input = new TaskInput();
            input.InternalFilenameToWriteResults = m_Results.WriteResults;
#if UNITY_2019_3_OR_NEWER
            input.InternalFilenameToAdditionalFiles = m_Content.AdditionalFiles;
#endif
            input.InternalFilenameToBundleName = m_WriteData.FileToBundle;
            input.GetCompressionForIdentifier = (x) => m_Parameters.GetCompressionForIdentifier(x);
            input.GetOutputFilePathForIdentifier = (x) => m_Parameters.GetOutputFilePathForIdentifier(x);
            input.BuildCache = m_Parameters.UseCache ? m_Cache : null;
            input.ProgressTracker = m_Tracker;
            input.TempOutputFolder = m_Parameters.TempOutputFolder;
            input.AssetToFilesDependencies = m_WriteData.AssetToFiles;
            input.Log = m_Log;

            input.Threaded = SupportsMultiThreadedArchiving && ScriptableBuildPipeline.threadedArchiving;

            TaskOutput output;
            ReturnCode code = Run(input, out output);

            if (code == ReturnCode.Success)
            {
                foreach (var item in output.BundleDetails)
                    m_Results.BundleInfos.Add(item.Key, item.Value);
            }

            return code;
        }

        static Dictionary<string, string[]> CalculateBundleDependencies(List<List<string>> assetFileList, Dictionary<string, string> filenameToBundleName)
        {
            var bundleDependencies = new Dictionary<string, string[]>();
            Dictionary<string, HashSet<string>> bundleDependenciesHash = new Dictionary<string, HashSet<string>>();
            foreach (var files in assetFileList)
            {
                if (files.IsNullOrEmpty())
                    continue;

                string bundle = filenameToBundleName[files.First()];
                HashSet<string> dependencies;
                bundleDependenciesHash.GetOrAdd(bundle, out dependencies);
                dependencies.UnionWith(files.Select(x => filenameToBundleName[x]));
                dependencies.Remove(bundle);
            }
            foreach (var dep in bundleDependenciesHash)
            {
                string[] ret = dep.Value.ToArray();
                Array.Sort(ret);
                bundleDependencies.Add(dep.Key, ret);
            }
            return bundleDependencies;
        }

        static void PostArchiveProcessing(List<ArchiveWorkItem> items, List<List<string>> assetFileList, Dictionary<string, string> filenameToBundleName, IBuildLogger log)
        {
            using (log.ScopedStep(LogLevel.Info, "PostArchiveProcessing"))
            {
                Dictionary<string, string[]> bundleDependencies = CalculateBundleDependencies(assetFileList, filenameToBundleName);
                foreach (ArchiveWorkItem item in items)
                {
                    // apply bundle dependencies
                    item.ResultDetails.Dependencies = bundleDependencies.ContainsKey(item.BundleName) ? bundleDependencies[item.BundleName] : new string[0];

                    // set the hash on the bundle result. must be applied here because the ToString of the Hash128 can't be called on a thread
                    item.ResultDetails.Hash = item.ResultHash;
                }
            }
        }

        static Dictionary<string, ulong> CalculateHashFileOffsets(TaskInput input)
        {
            using (input.Log.ScopedStep(LogLevel.Info, "CalculateHashFileOffsets"))
            {
                Dictionary<string, ulong> fileOffsets = new Dictionary<string, ulong>();
                foreach (var pair in input.InternalFilenameToWriteResults)
                {
                    foreach (ResourceFile serializedFile in pair.Value.resourceFiles)
                    {
                        if (!serializedFile.serializedFile)
                            continue;

                        ObjectSerializedInfo firstObject = pair.Value.serializedObjects.First(x => x.header.fileName == serializedFile.fileAlias);
                        fileOffsets[serializedFile.fileName] = firstObject.header.offset;
                    }
                }
                return fileOffsets;
            }
        }

        static List<ArchiveWorkItem> CreateWorkItems(TaskInput input)
        {
            using (input.Log.ScopedStep(LogLevel.Info, "CreateWorkItems"))
            {
                List<KeyValuePair<string, List<ResourceFile>>> bundleResources;
                Dictionary<string, List<ResourceFile>> bundleToResources = new Dictionary<string, List<ResourceFile>>();
                foreach (var pair in input.InternalFilenameToWriteResults)
                {
                    string bundle = input.InternalFilenameToBundleName[pair.Key];
                    List<ResourceFile> resourceFiles;
                    bundleToResources.GetOrAdd(bundle, out resourceFiles);
                    resourceFiles.AddRange(pair.Value.resourceFiles);
                }
#if UNITY_2019_3_OR_NEWER
	            foreach (var pair in input.InternalFilenameToAdditionalFiles)
	            {
	                List<ResourceFile> resourceFiles;
	                bundleToResources.GetOrAdd(pair.Key, out resourceFiles);
	                foreach (var file in pair.Value)
	                {
	                    resourceFiles.Add(file);
	                    input.InternalFilenameToBundleName[file.fileAlias] = pair.Key;
	                }
	            }
#endif
                bundleResources = bundleToResources.ToList();

                List<ArchiveWorkItem> allItems = bundleResources.Select((x, index) =>
                    new ArchiveWorkItem
                    {
                        Index = index,
                        BundleName = x.Key,
                        ResourceFiles = x.Value.ToArray(),
                        Compression = input.GetCompressionForIdentifier(x.Key),
                        OutputFilePath = input.GetOutputFilePathForIdentifier(x.Key)
                    }
                ).ToList();

                return allItems;
            }
        }

        static internal ReturnCode Run(TaskInput input, out TaskOutput output)
        {
            output = new TaskOutput();
            output.BundleDetails = new Dictionary<string, BundleDetails>();

            List<ArchiveWorkItem> allItems = CreateWorkItems(input);
            Dictionary<string, ulong> fileOffsets = CalculateHashFileOffsets(input);

            IList<CacheEntry> cacheEntries = null;
            IList<CachedInfo> cachedInfo = null;
            List<ArchiveWorkItem> cachedItems = new List<ArchiveWorkItem>();
            List<ArchiveWorkItem> nonCachedItems = allItems;
            if (input.BuildCache != null)
            {
                using (input.Log.ScopedStep(LogLevel.Info, "Creating Cache Entries"))
                    cacheEntries = allItems.Select(x => GetCacheEntry(input.BuildCache, x.BundleName, x.ResourceFiles, x.Compression)).ToList();

                using (input.Log.ScopedStep(LogLevel.Info, "Load Cached Data"))
                    input.BuildCache.LoadCachedData(cacheEntries, out cachedInfo);

                cachedItems = allItems.Where(x => cachedInfo[x.Index] != null).ToList();
                nonCachedItems = allItems.Where(x => cachedInfo[x.Index] == null).ToList();
                foreach(ArchiveWorkItem i in allItems)
                    i.CachedArtifactPath = string.Format("{0}/{1}", input.BuildCache.GetCachedArtifactsDirectory(cacheEntries[i.Index]), i.BundleName);
            }

            using (input.Log.ScopedStep(LogLevel.Info, "CopyingCachedFiles"))
            {
                foreach (ArchiveWorkItem item in cachedItems)
                {
                    if (!input.ProgressTracker.UpdateInfoUnchecked(string.Format("{0} (Cached)", item.BundleName)))
                        return ReturnCode.Canceled;

                    item.ResultDetails = (BundleDetails)cachedInfo[item.Index].Data[0];
                    item.ResultDetails.FileName = item.OutputFilePath;
                    item.ResultHash = item.ResultDetails.Hash;
                    CopyFileWithTimestampIfDifferent(item.CachedArtifactPath, item.ResultDetails.FileName, input.Log);
                }
            }

            // Write all the files that aren't cached
            if (!ArchiveItems(nonCachedItems, fileOffsets, input.TempOutputFolder, input.ProgressTracker, input.Threaded, input.Log))
                return ReturnCode.Canceled;

            PostArchiveProcessing(allItems, input.AssetToFilesDependencies.Values.ToList(), input.InternalFilenameToBundleName, input.Log);

            // Put everything into the cache
            if (input.BuildCache != null)
            {
                using (input.Log.ScopedStep(LogLevel.Info, "Copying To Cache"))
                {
                    List<CachedInfo> uncachedInfo = nonCachedItems.Select(x => GetCachedInfo(input.BuildCache, cacheEntries[x.Index], x.ResourceFiles, x.ResultDetails)).ToList();
                    input.BuildCache.SaveCachedData(uncachedInfo);
                }
            }

            output.BundleDetails = allItems.ToDictionary((x) => x.BundleName, (x) => x.ResultDetails);

            if (input.OutCachedBundles != null)
                input.OutCachedBundles.AddRange(cachedItems.Select(x => x.BundleName));

            return ReturnCode.Success;
        }

        static private void ArchiveSingleItem(ArchiveWorkItem item, Dictionary<string, ulong> fileOffsets, string tempOutputFolder, IBuildLogger log)
        {
            using (log.ScopedStep(LogLevel.Info, $"Archive {item.BundleName}"))
            {
                item.ResultDetails = new BundleDetails();
                string writePath = string.Format("{0}/{1}", tempOutputFolder, item.BundleName);
                if (!string.IsNullOrEmpty(item.CachedArtifactPath))
                    writePath = item.CachedArtifactPath;

                Directory.CreateDirectory(Path.GetDirectoryName(writePath));
                item.ResultDetails.FileName = item.OutputFilePath;
                item.ResultDetails.Crc = ContentBuildInterface.ArchiveAndCompress(item.ResourceFiles, writePath, item.Compression);
                item.ResultHash = CalculateHashVersion(fileOffsets, item.ResourceFiles, item.ResultDetails.Dependencies);
                CopyFileWithTimestampIfDifferent(writePath, item.ResultDetails.FileName, log);
            }
        }

        static private bool ArchiveItems(List<ArchiveWorkItem> items, Dictionary<string, ulong> fileOffsets, string tempOutputFolder, IProgressTracker tracker, bool threaded, IBuildLogger log)
        {
            using (log.ScopedStep(LogLevel.Info, "ArchiveItems", threaded))
            {
                log?.AddEntry(LogLevel.Info, $"Archiving {items.Count} Bundles");
                if (threaded)
                    return ArchiveItemsThreaded(items, fileOffsets, tempOutputFolder, tracker, log);

                foreach (ArchiveWorkItem item in items)
                {
                    if (tracker != null && !tracker.UpdateInfoUnchecked(item.BundleName))
                        return false;

                    ArchiveSingleItem(item, fileOffsets, tempOutputFolder, log);
                }
                return true;
            }
        }

        static private bool ArchiveItemsThreaded(List<ArchiveWorkItem> items, Dictionary<string, ulong> fileOffsets, string tempOutputFolder, IProgressTracker tracker, IBuildLogger log)
        {
            CancellationTokenSource srcToken = new CancellationTokenSource();

            SemaphoreSlim semaphore = new SemaphoreSlim(0);
            List<Task> tasks = new List<Task>(items.Count);
            foreach (ArchiveWorkItem item in items)
            {
                tasks.Add(Task.Run(() =>
                {
                    try { ArchiveSingleItem(item, fileOffsets, tempOutputFolder, log); }
                    finally { semaphore.Release(); }
                }, srcToken.Token));
            }

            for (int i = 0; i < items.Count; i++)
            {
                semaphore.Wait(srcToken.Token);
                if (tracker != null && !tracker.UpdateInfoUnchecked($"Archive {i+1}/{items.Count}"))
                {
                    srcToken.Cancel();
                    break;
                }
            }
            Task.WaitAny(Task.WhenAll(tasks));

            return !srcToken.Token.IsCancellationRequested;
        }
    }
}
