#if JENKINS
#if UNITY_2020_2_OR_NEWER
using NUnit.Framework;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.TestTools;
using UnityEngine.XR.Management;
using UnityEngine.XR.WindowsMR;

#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.Build.Reporting;
using UnityEditor.SceneManagement;
using UnityEditor.XR.Management;
using UnityEditor.XR.Management.Metadata;


namespace UnityEditor.XR.WindowsMR.Tests
{

    class BuildTests : TestBaseSetup
    {
        private bool SetEnableLoderForTarget(BuildTargetGroup buildTargetGroup, bool enable)
        {
            if (buildTargetGroup != BuildTargetGroup.Standalone && buildTargetGroup != BuildTargetGroup.WSA)
                return false;

            XRGeneralSettings settings = XRGeneralSettingsPerBuildTarget.XRGeneralSettingsForBuildTarget(buildTargetGroup);
            if (settings == null)
                return false;

            bool ret = false;

            if (enable)
            {
                ret = XRPackageMetadataStore.AssignLoader(settings.Manager, typeof(WindowsMRLoader).Name, buildTargetGroup);
            }
            else
            {
                ret = XRPackageMetadataStore.RemoveLoader(settings.Manager, typeof(WindowsMRLoader).Name, buildTargetGroup);
            }

            return ret;
        }

        [Test]
        public void CheckBinariesFilteredIfNotEnabledInBuildTarget()
        {
            bool ret = SetEnableLoderForTarget(BuildTargetGroup.Standalone, false);
            Assert.IsTrue(ret);

            PlayerSettings.SetScriptingBackend(BuildTargetGroup.Standalone, ScriptingImplementation.IL2CPP);

            string tempPath = Path.Combine(Path.GetTempPath(), "TestBuildFolder");
            if (Directory.Exists(tempPath))
            {
                Directory.Delete(tempPath, true);
            }
            Directory.CreateDirectory(tempPath);

            string buildFolder = Path.Combine(tempPath, $"{PlayerSettings.productName}.exe");

            BuildPlayerOptions buildPlayerOptions = new BuildPlayerOptions();
            buildPlayerOptions.scenes = new[] { "Assets/BlankScene.unity" };
            buildPlayerOptions.locationPathName = buildFolder;
            buildPlayerOptions.target = BuildTarget.StandaloneWindows64;
            buildPlayerOptions.targetGroup = BuildTargetGroup.Standalone;
            buildPlayerOptions.options = BuildOptions.None;

            BuildReport report = BuildPipeline.BuildPlayer(buildPlayerOptions);
            BuildSummary summary = report.summary;


            string wmrDllFile = Path.Combine(tempPath, $"{PlayerSettings.productName}_Data", "Plugins", "x86_64", "WindowsMRXRSDK.dll");
            Assert.IsFalse(File.Exists(wmrDllFile));
        }

    }
}
#endif //UNITY_EDITOR
#endif //UNITY_2020_2_OR_NEWER
#endif //JENKINS
