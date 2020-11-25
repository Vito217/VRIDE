using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using UnityEditor;
using UnityEditor.Build;
using UnityEditor.Build.Reporting;
using UnityEditor.XR.Management;

using UnityEngine;
using UnityEngine.XR.Management;

using UnityEngine.XR.WindowsMR;

namespace UnityEditor.XR.WindowsMR
{
    /// <summary>
    /// Small utility class for reading, updating and writing boot config.
    /// </summary>
    class BootConfig
    {
        struct BootConfigEntry
        {
            public string key;
            public string value;
        }

        private List<BootConfigEntry> bootConfigEntries;

        private string GetPathToBootConfig(BuildReport report)
        {
            string bootConfigPath = report.summary.outputPath;

            // Boot Config data path is highly specific to the platform being built.
            if (report.summary.platformGroup == BuildTargetGroup.WSA)
            {
                bootConfigPath = Path.Combine(bootConfigPath, PlayerSettings.productName);
                bootConfigPath = Path.Combine(bootConfigPath, "Data");
                bootConfigPath = Path.Combine(bootConfigPath, "boot.config");
            }
            else
            {
                bootConfigPath = bootConfigPath.Substring(0, bootConfigPath.Length - ($"{PlayerSettings.productName}.exe".Length + 1));
                bootConfigPath = Path.Combine(bootConfigPath, $"{PlayerSettings.productName}_Data");
                bootConfigPath = Path.Combine(bootConfigPath, "boot.config");
            }

            return bootConfigPath;
        }

        private BuildReport buildReport;
        private string bootConfigPath;

        public BootConfig(BuildReport report)
        {
            buildReport = report;
            bootConfigPath = GetPathToBootConfig(report);

        }

        public void ReadBootConfg()
        {
            bootConfigEntries = new List<BootConfigEntry>();
            using (StreamReader sr = File.OpenText(bootConfigPath))
            {
                string entry_text;
                while ((entry_text = sr.ReadLine()) != null)
                {
                    if (String.IsNullOrEmpty(entry_text))
                        continue;

                    var idx = entry_text.IndexOf('=');
                    if (idx < 0)
                        continue;

                    var key = entry_text.Substring(0, idx);
                    var value = entry_text.Substring(idx + 1, entry_text.Length - (idx + 1));
                    bootConfigEntries.Add(new BootConfigEntry()
                    {
                        key = key,
                        value = value
                    });

                }
            }

        }

        public void SetValueForKey(string key, string value, bool replace = false)
        {
            if (replace)
            {
                bootConfigEntries = bootConfigEntries.Where((bce) => String.Compare(bce.key, key) != 0).ToList();
            }
            bootConfigEntries.Add(new BootConfigEntry() { key = key, value = value });
        }

        public void WriteBootConfig()
        {
            using (FileStream fs = File.OpenWrite(bootConfigPath))
            {
                fs.SetLength(0);

                using (StreamWriter sw = new StreamWriter(fs))
                {
                    foreach (var bce in bootConfigEntries)
                    {
                        sw.WriteLine($"{bce.key}={bce.value}");
                    }
                }
            }
        }
    }

    /// <summary>Build Processor class used to handle XR Plugin specific build actions/</summary>
    /// <typeparam name="WindowsMRSettings">The settings instance type the build processor will use.</typeparam>
    public class WindowsMRBuildProcessor : XRBuildHelper<WindowsMRSettings>
    {
        private static List<BuildTarget> s_ValidBuildTargets = new List<BuildTarget>(){
            BuildTarget.StandaloneWindows,
            BuildTarget.StandaloneWindows64,
            BuildTarget.WSAPlayer,
        };

        public override string BuildSettingsKey { get { return Constants.k_SettingsKey; } }

        private bool IsCurrentBuildTargetVaild(BuildReport report)
        {
            return report.summary.platformGroup == BuildTargetGroup.WSA ||
                (report.summary.platformGroup == BuildTargetGroup.Standalone && s_ValidBuildTargets.Contains(report.summary.platform));
        }

        private bool HasLoaderEnabledForTarget(BuildTargetGroup buildTargetGroup)
        {
            if (buildTargetGroup != BuildTargetGroup.Standalone && buildTargetGroup != BuildTargetGroup.WSA)
                return false;

            XRGeneralSettings settings = XRGeneralSettingsPerBuildTarget.XRGeneralSettingsForBuildTarget(buildTargetGroup);
            if (settings == null)
                return false;

            bool loaderFound = false;
            for (int i = 0; i < settings.Manager.loaders.Count; ++i)
            {
                if (settings.Manager.loaders[i] as WindowsMRLoader != null)
                {
                    loaderFound = true;
                    break;
                }
            }

            return loaderFound;
        }

        private WindowsMRPackageSettings PackageSettingsForBuildTargetGroup(BuildTargetGroup buildTargetGroup)
        {
            if (!HasLoaderEnabledForTarget(buildTargetGroup))
                return null;

            UnityEngine.Object settingsObj = null;
            EditorBuildSettings.TryGetConfigObject(BuildSettingsKey, out settingsObj);
            WindowsMRPackageSettings settings = settingsObj as WindowsMRPackageSettings;

            if (settings == null)
            {
                var assets = AssetDatabase.FindAssets("t:WindowsMRPackageSettings");
                if (assets.Length == 1)
                {
                    string path = AssetDatabase.GUIDToAssetPath(assets[0]);
                    settings = AssetDatabase.LoadAssetAtPath(path, typeof(WindowsMRPackageSettings)) as WindowsMRPackageSettings;
                    if (settings != null)
                    {
                        EditorBuildSettings.AddConfigObject(BuildSettingsKey, settings, true);
                    }

                }
            }

            return settings;
        }

        /// <summary>Get the XR Plugin build settings for the specific build platform.</summary>
        /// <param name="buildTargetGroup">The build platform we want to get settings for.</param>
        /// <returns>An instance of WindowsMRBuildSettings, or null if there are none for the current build platform.</returns>
        public WindowsMRBuildSettings BuildSettingsForBuildTargetGroup(BuildTargetGroup buildTargetGroup)
        {
            WindowsMRPackageSettings settings = PackageSettingsForBuildTargetGroup(buildTargetGroup);

            if (settings != null)
            {
                WindowsMRBuildSettings targetSettings = settings.GetBuildSettingsForBuildTargetGroup(buildTargetGroup);
                return targetSettings;
            }

            return null;
        }

        /// <summary>Get a generic object reference for runtime settings for the build platform</summary>
        /// <param name="buildTargetGroup">The build platform we want to get settings for.</param>
        /// <returns>An object instance of the saved settings, or null if there are none.</returns>
        public override UnityEngine.Object SettingsForBuildTargetGroup(BuildTargetGroup buildTargetGroup)
        {
            WindowsMRPackageSettings settings = PackageSettingsForBuildTargetGroup(buildTargetGroup);

            if (settings != null)
            {
                WindowsMRSettings targetSettings = settings.GetSettingsForBuildTargetGroup(buildTargetGroup);
                return targetSettings;
            }

            return null;
        }

        const string k_ForcePrimaryWindowHolographic = "force-primary-window-holographic";
        const string k_VrEnabled = "vr-enabled";
        const string k_WmrLibrary = "xrsdk-windowsmr-library";
        const string k_WmrLibraryName = "WindowsMRXRSDK";
        const string k_EarlyBootHolographic = "early-boot-windows-holographic";

        /// <summary>OnPostprocessBuild override to provide XR Plugin specific build actions.</summary>
        /// <param name="report">The build report.</param>
        public override void OnPostprocessBuild(BuildReport report)
        {
            if (!IsCurrentBuildTargetVaild(report))
                return;

            if (!HasLoaderEnabledForTarget(report.summary.platformGroup))
                return;
            
            base.OnPostprocessBuild(report);

            BootConfig bootConfig = new BootConfig(report);
            bootConfig.ReadBootConfg();

            bootConfig.SetValueForKey(k_VrEnabled, "1", true);
            bootConfig.SetValueForKey(k_WmrLibrary, k_WmrLibraryName, true);
            if (report.summary.platformGroup == BuildTargetGroup.WSA)
            {
                bootConfig.SetValueForKey(k_EarlyBootHolographic, "1", true);

                bool usePrimaryWindow = true;
                WindowsMRBuildSettings buildSettings = BuildSettingsForBuildTargetGroup(report.summary.platformGroup);
                if (buildSettings != null)
                {
                    usePrimaryWindow = buildSettings.UsePrimaryWindowForDisplay;
                }
                
                bootConfig.SetValueForKey(k_ForcePrimaryWindowHolographic, usePrimaryWindow ? "1" : "0", true);
            }

            bootConfig.WriteBootConfig();
        }

        private readonly string[] runtimePluginNames = new string[]
        {
            "WindowsMRXRSDK.dll",
        };

        public bool ShouldIncludeRuntimePluginsInBuild(string path)
        {
            return HasLoaderEnabledForTarget(BuildPipeline.GetBuildTargetGroup(EditorUserBuildSettings.activeBuildTarget));
        }

        private readonly string spatializerPluginName = "AudioPluginMsHRTF.dll";
        private readonly string spatializerReadableName = "MS HRTF Spatializer";

        public bool ShouldIncludeSpatializerPluginsInBuild(string path)
        {
            string currentSpatializerPluginName = AudioSettings.GetSpatializerPluginName();

            if (string.Compare(spatializerReadableName, currentSpatializerPluginName, true) == 0)
                return true;

            return false;
        }

        private readonly string[] remotingPluginNames = new string[]
        {
            "Microsoft.Holographic.AppRemoting.dll",
            "PerceptionDevice.dll",
            "UnityRemotingWMR.dll"
        };

        public bool ShouldIncludeRemotingPluginsInBuild(string path)
        {
            BuildTargetGroup buildTargetGroup = BuildPipeline.GetBuildTargetGroup(EditorUserBuildSettings.activeBuildTarget);
            WindowsMRBuildSettings buildSettings = BuildSettingsForBuildTargetGroup(buildTargetGroup) as WindowsMRBuildSettings;
            if (buildSettings == null)
                return false;

            return buildSettings.HolographicRemoting;
        }

        /// <summary>OnPreprocessBuild override to provide XR Plugin specific build actions.</summary>
        /// <param name="report">The build report.</param>
        public override void OnPreprocessBuild(BuildReport report)
        {
            if (IsCurrentBuildTargetVaild(report) && HasLoaderEnabledForTarget(report.summary.platformGroup))
                base.OnPreprocessBuild(report);

            var allPlugins = PluginImporter.GetAllImporters();
            foreach (var plugin in allPlugins)
            {
                if (plugin.isNativePlugin)
                {
                    foreach (var pluginName in remotingPluginNames)
                    {
                        if (plugin.assetPath.Contains(pluginName))
                        {
                            plugin.SetIncludeInBuildDelegate(ShouldIncludeRemotingPluginsInBuild);
                            break;
                        }
                    }

                    foreach (var pluginName in runtimePluginNames)
                    {
                        if (plugin.assetPath.Contains(pluginName))
                        {
                            plugin.SetIncludeInBuildDelegate(ShouldIncludeRuntimePluginsInBuild);
                            break;
                        }
                    }

                    if (plugin.assetPath.Contains(spatializerPluginName))
                    {
                        plugin.SetIncludeInBuildDelegate(ShouldIncludeSpatializerPluginsInBuild);
                    }
                }
            }
        }
    }
}
