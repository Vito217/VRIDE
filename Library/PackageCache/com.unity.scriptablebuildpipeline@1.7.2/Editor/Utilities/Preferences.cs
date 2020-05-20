using UnityEngine;
using System.Threading;
using System.Collections.Generic;
using System.IO;
using UnityEditor.Build.Pipeline.Tasks;

namespace UnityEditor.Build.Pipeline.Utilities
{
    static class ScriptableBuildPipeline
    {
        private class GUIScope : GUI.Scope
        {
            float m_LabelWidth;
            public GUIScope(float layoutMaxWidth)
            {
                m_LabelWidth = EditorGUIUtility.labelWidth;
                EditorGUIUtility.labelWidth = 250;
                GUILayout.BeginHorizontal();
                GUILayout.Space(10);
                GUILayout.BeginVertical();
                GUILayout.Space(15);
            }

            public GUIScope() : this(500)
            {
            }

            protected override void CloseScope()
            {
                GUILayout.EndVertical();
                GUILayout.EndHorizontal();
                EditorGUIUtility.labelWidth = m_LabelWidth;
            }
        }

        internal class Properties
        {
            public static readonly GUIContent generalSettings = EditorGUIUtility.TrTextContent("General Settings");
            public static readonly GUIContent threadedArchiving = EditorGUIUtility.TrTextContent("Threaded Archiving", "Thread the archiving and compress build stage.");
            public static readonly GUIContent logCacheMiss = EditorGUIUtility.TrTextContent("Log Cache Miss", "Log a warning on build cache misses. Warning will contain which asset and dependency caused the miss.");
            public static readonly GUIContent slimWriteResults = EditorGUIUtility.TrTextContent("Slim Write Results", "Reduces the caching of WriteResults data down to the bare minimum for improved cache performance.");
            public static readonly GUIContent maxCacheSize = EditorGUIUtility.TrTextContent("Maximum Cache Size (GB)", "The size of the Build Cache folder will be kept below this maximum value when possible.");
            public static readonly GUIContent buildCache = EditorGUIUtility.TrTextContent("Build Cache");
            public static readonly GUIContent purgeCache = EditorGUIUtility.TrTextContent("Purge Cache");
            public static readonly GUIContent pruneCache = EditorGUIUtility.TrTextContent("Prune Cache");
            public static readonly GUIContent cacheSizeIs = EditorGUIUtility.TrTextContent("Cache size is");
            public static readonly GUIContent pleaseWait = EditorGUIUtility.TrTextContent("Please wait...");
            public static bool startedCalculation = false;
            public static long currentCacheSize = -1;
        }

        [System.Serializable]
        internal class Settings
        {
            public bool threadedArchiving = true;
            public bool logCacheMiss = false;
            public bool slimWriteResults = true;
            public int maximumCacheSize = 200;
        }

        internal static Settings s_Settings = new Settings();

        public static bool threadedArchiving => s_Settings.threadedArchiving;

        public static bool logCacheMiss => s_Settings.logCacheMiss;
        public static bool slimWriteResults => s_Settings.slimWriteResults;
        public static int maximumCacheSize => s_Settings.maximumCacheSize;

        internal static void LoadSettings()
        {
            // Load old settings
            s_Settings.threadedArchiving = EditorPrefs.GetBool("ScriptableBuildPipeline.threadedArchiving", s_Settings.threadedArchiving);
            s_Settings.logCacheMiss = EditorPrefs.GetBool("ScriptableBuildPipeline.logCacheMiss", s_Settings.logCacheMiss);
            s_Settings.maximumCacheSize = EditorPrefs.GetInt("BuildCache.maximumSize", s_Settings.maximumCacheSize);

            // Load new settings from Json
            if (File.Exists("ProjectSettings/ScriptableBuildPipeline.json"))
            {
                var json = File.ReadAllText("ProjectSettings/ScriptableBuildPipeline.json");
                EditorJsonUtility.FromJsonOverwrite(json, s_Settings);
            }
        }

        internal static void SaveSettings()
        {
            var json = EditorJsonUtility.ToJson(s_Settings, true);
            File.WriteAllText("ProjectSettings/ScriptableBuildPipeline.json", json);
        }

        static ScriptableBuildPipeline()
        {
            LoadSettings();
        }

#if UNITY_2019_1_OR_NEWER
        [SettingsProvider]
        static SettingsProvider CreateBuildCacheProvider()
        {
            var provider = new SettingsProvider("Preferences/Scriptable Build Pipeline", SettingsScope.User, SettingsProvider.GetSearchKeywordsFromGUIContentProperties<Properties>());
            provider.guiHandler = sarchContext => OnGUI();
            return provider;
        }
#else
        [PreferenceItem("Scriptable Build Pipeline")]
#endif
        static void OnGUI()
        {
            using (new GUIScope())
            {
                EditorGUI.BeginChangeCheck();
                DrawProperties();
                if (EditorGUI.EndChangeCheck())
                    SaveSettings();
            }
        }

        static void DrawProperties()
        {
            GUILayout.Label(Properties.generalSettings, EditorStyles.boldLabel);

            if (ArchiveAndCompressBundles.SupportsMultiThreadedArchiving)
                s_Settings.threadedArchiving = EditorGUILayout.Toggle(Properties.threadedArchiving, s_Settings.threadedArchiving);

            s_Settings.logCacheMiss = EditorGUILayout.Toggle(Properties.logCacheMiss, s_Settings.logCacheMiss);
            s_Settings.slimWriteResults = EditorGUILayout.Toggle(Properties.slimWriteResults, s_Settings.slimWriteResults);

            GUILayout.Space(15);
            GUILayout.Label(Properties.buildCache, EditorStyles.boldLabel);
            // Show Gigabytes to the user.
            const int kMinSizeInGigabytes = 1;
            const int kMaxSizeInGigabytes = 200;

            // Write size in GigaBytes.
            s_Settings.maximumCacheSize = EditorGUILayout.IntSlider(Properties.maxCacheSize, s_Settings.maximumCacheSize, kMinSizeInGigabytes, kMaxSizeInGigabytes);

            GUILayout.BeginHorizontal(GUILayout.MaxWidth(500));
            if (GUILayout.Button(Properties.purgeCache, GUILayout.Width(120)))
            {
                BuildCache.PurgeCache(true);
                Properties.startedCalculation = false;
            }

            if (GUILayout.Button(Properties.pruneCache, GUILayout.Width(120)))
            {
                BuildCache.PruneCache();
                Properties.startedCalculation = false;
            }
            GUILayout.EndHorizontal();

            // Current cache size
            if (!Properties.startedCalculation)
            {
                Properties.startedCalculation = true;
                ThreadPool.QueueUserWorkItem((state) =>
                {
                    BuildCache.ComputeCacheSizeAndFolders(out Properties.currentCacheSize, out List<BuildCache.CacheFolder> cacheFolders);
                });
            }

            if (Properties.currentCacheSize >= 0)
                GUILayout.Label(Properties.cacheSizeIs.text + " " + EditorUtility.FormatBytes(Properties.currentCacheSize));
            else
                GUILayout.Label(Properties.cacheSizeIs.text + " is being calculated...");
        }
    }
}
