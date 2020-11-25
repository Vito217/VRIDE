using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.XR.Management;
using UnityEngine.XR;
#if UNITY_INPUT_SYSTEM
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.Layouts;
using UnityEngine.InputSystem.XR;
using Unity.XR.Oculus.Input;
#endif

#if UNITY_EDITOR
using System.IO;
using System.Linq;
using UnityEditor;
#endif


namespace Unity.XR.Oculus
{
#if UNITY_INPUT_SYSTEM
#if UNITY_EDITOR
    [InitializeOnLoad]
#endif
    static class InputLayoutLoader
    {
        static InputLayoutLoader()
        {
            RegisterInputLayouts();
        }

        public static void RegisterInputLayouts()
        {
            InputSystem.RegisterLayout<OculusHMD>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct("^(Oculus Rift)|^(Oculus Quest)|^(Oculus Go)"));
            InputSystem.RegisterLayout<OculusTouchController>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct(@"(^(Oculus Touch Controller))|(^(Oculus Quest Controller))"));
            InputSystem.RegisterLayout<OculusRemote>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct(@"Oculus Remote"));
            InputSystem.RegisterLayout<OculusTrackingReference>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct(@"((Tracking Reference)|(^(Oculus Rift [a-zA-Z0-9]* \(Camera)))"));
            InputSystem.RegisterLayout<OculusGoController>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct("^(Oculus Tracked Remote)"));

            InputSystem.RegisterLayout<OculusHMDExtended>();
            InputSystem.RegisterLayout<GearVRTrackedController>();
        }
    }
#endif

    public class OculusLoader : XRLoaderHelper
#if UNITY_EDITOR
    , IXRLoaderPreInit
#endif
    {
        private static List<XRDisplaySubsystemDescriptor> s_DisplaySubsystemDescriptors = new List<XRDisplaySubsystemDescriptor>();
        private static List<XRInputSubsystemDescriptor> s_InputSubsystemDescriptors = new List<XRInputSubsystemDescriptor>();

        public XRDisplaySubsystem displaySubsystem
        {
            get
            {
                return GetLoadedSubsystem<XRDisplaySubsystem>();
            }
        }

        public XRInputSubsystem inputSubsystem
        {
            get
            {
                return GetLoadedSubsystem<XRInputSubsystem>();
            }
        }

        public override bool Initialize()
        {
#if (UNITY_EDITOR && !UNITY_EDITOR_WIN) || (UNITY_STANDALONE && !UNITY_STANDALONE_WIN)
            return false;
#else

#if UNITY_INPUT_SYSTEM
            InputLayoutLoader.RegisterInputLayouts();
#endif // UNITY_INPUT_SYSTEM

            OculusSettings settings = GetSettings();
            if (settings != null)
            {
                UserDefinedSettings userDefinedSettings;
                userDefinedSettings.sharedDepthBuffer = (ushort)(settings.SharedDepthBuffer ? 1 : 0);
                userDefinedSettings.dashSupport = (ushort)(settings.DashSupport ? 1 : 0);
                userDefinedSettings.stereoRenderingMode = (ushort)settings.GetStereoRenderingMode();
                userDefinedSettings.colorSpace = (ushort)((QualitySettings.activeColorSpace == ColorSpace.Linear) ? 1 : 0);
                userDefinedSettings.lowOverheadMode = (ushort)(settings.LowOverheadMode ? 1 : 0);
                userDefinedSettings.protectedContext = (ushort)(settings.ProtectedContext ? 1 : 0);
                userDefinedSettings.focusAware = (ushort)(settings.FocusAware ? 1 : 0);
                userDefinedSettings.optimizeBufferDiscards = (ushort)(settings.OptimizeBufferDiscards ? 1 : 0);
                SetUserDefinedSettings(userDefinedSettings);
            }

            CreateSubsystem<XRDisplaySubsystemDescriptor, XRDisplaySubsystem>(s_DisplaySubsystemDescriptors, "oculus display");
            CreateSubsystem<XRInputSubsystemDescriptor, XRInputSubsystem>(s_InputSubsystemDescriptors, "oculus input");

            if (displaySubsystem == null || inputSubsystem == null)
            {
                Debug.LogError("Unable to start Oculus XR Plugin.");
            }

            if (displaySubsystem == null)
            {
                Debug.LogError("Failed to load display subsystem.");
            }

            if (inputSubsystem == null)
            {
                Debug.LogError("Failed to load input subsystem.");
            }

            return displaySubsystem != null && inputSubsystem != null;
#endif // (UNITY_EDITOR && !UNITY_EDITOR_WIN) || (UNITY_STANDALONE && !UNITY_STANDALONE_WIN)
        }

        public override bool Start()
        {
            StartSubsystem<XRDisplaySubsystem>();
            StartSubsystem<XRInputSubsystem>();

            return true;
        }

        public override bool Stop()
        {
            StopSubsystem<XRDisplaySubsystem>();
            StopSubsystem<XRInputSubsystem>();

            return true;
        }

        public override bool Deinitialize()
        {
            DestroySubsystem<XRDisplaySubsystem>();
            DestroySubsystem<XRInputSubsystem>();

            return true;
        }

#if UNITY_EDITOR_WIN
        [InitializeOnLoadMethod]
        static void EditorLoadOVRPlugin()
        {
            string ovrpPath = "";

            // loop over all the native plugin importers and find the OVRPlugin the editor should use
            var importers = PluginImporter.GetAllImporters();
            foreach (var importer in importers)
            {
                if (!importer.GetCompatibleWithEditor() || !importer.assetPath.Contains("OVRPlugin"))
                    continue;

                if (!importer.GetCompatibleWithPlatform(BuildTarget.StandaloneWindows64) || !importer.assetPath.EndsWith(".dll"))
                    continue;

                ovrpPath = importer.assetPath;

                if (!importer.GetIsOverridable())
                    break;
            }

            if (ovrpPath == "")
            {
                Debug.LogError("Couldn't locate OVRPlugin.dll");
                return;
            }

            if (!LoadOVRPlugin(AssetPathToAbsolutePath(ovrpPath)))
            {
                Debug.LogError("Failed to load OVRPlugin.dll");
                return;
            }
        }

        static string AssetPathToAbsolutePath(string assetPath)
        {
            var path = assetPath.Replace('/', Path.DirectorySeparatorChar);

            if (assetPath.StartsWith("Packages"))
            {
                path = String.Join(Path.DirectorySeparatorChar.ToString(), path.Split(Path.DirectorySeparatorChar).Skip(2));

                return Path.Combine(UnityEditor.PackageManager.PackageInfo.FindForAssetPath(assetPath).resolvedPath, path);
            }
            else
            {
                string assetsPath = Application.dataPath;
                assetsPath = assetsPath.Replace('/', Path.DirectorySeparatorChar);

                if (assetsPath.EndsWith("Assets"))
                {
                    assetsPath = assetsPath.Substring(0, assetsPath.LastIndexOf("Assets"));
                }

                return Path.Combine(assetsPath, assetPath);
            }
        }
#elif UNITY_STANDALONE_WIN || (UNITY_ANDROID && !UNITY_EDITOR)
        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.AfterAssembliesLoaded)]
        static void RuntimeLoadOVRPlugin()
        {
            if (!LoadOVRPlugin(""))
            {
                Debug.LogError("Failed to load OVRPlugin.dll");
            }
        }
#endif

#if UNITY_EDITOR && XR_MGMT_GTE_320
        private void RemoveVulkanFromAndroidGraphicsAPIs()
        {
            // don't need to do anything if auto apis is selected
            if (PlayerSettings.GetUseDefaultGraphicsAPIs(BuildTarget.Android))
                return;

            GraphicsDeviceType[] oldApis = PlayerSettings.GetGraphicsAPIs(BuildTarget.Android);
            List<GraphicsDeviceType> newApisList = new List<GraphicsDeviceType>();
            bool vulkanRemoved = false;

            // copy all entries except vulkan
            foreach (GraphicsDeviceType dev in oldApis)
            {
                if (dev == GraphicsDeviceType.Vulkan)
                {
                    vulkanRemoved = true;
                    continue;
                }

                newApisList.Add(dev);
            }

            // if we didn't remove Vulkan from the list, no need to do any further processing
            if (vulkanRemoved == false)
                return;

            if (newApisList.Count <= 0)
            {
                newApisList.Add(GraphicsDeviceType.OpenGLES3);
                Debug.LogWarning(
                    "Vulkan is currently experimental on Oculus Quest. It has been removed from your list of Android graphics APIs and replaced with OpenGLES3.\n" +
                    "If you would like to use experimental Quest Vulkan support, you can add it back into the list of graphics APIs in the Player settings.");
            }
            else
            {
                Debug.LogWarning(
                    "Vulkan is currently experimental on Oculus Quest. It has been removed from your list of Android graphics APIs.\n" +
                    "If you would like to use experimental Quest Vulkan support, you can add it back into the list of graphics APIs in the Player settings.");
            }

            PlayerSettings.SetGraphicsAPIs(BuildTarget.Android, newApisList.ToArray());
        }

        public override void WasAssignedToBuildTarget(BuildTargetGroup buildTargetGroup)
        {
            if (buildTargetGroup == BuildTargetGroup.Android)
            {
                RemoveVulkanFromAndroidGraphicsAPIs();
            }
        }
#endif

        [StructLayout(LayoutKind.Sequential)]
        struct UserDefinedSettings
        {
            public ushort sharedDepthBuffer;
            public ushort dashSupport;
            public ushort stereoRenderingMode;
            public ushort colorSpace;
            public ushort lowOverheadMode;
            public ushort protectedContext;
            public ushort focusAware;
            public ushort optimizeBufferDiscards;
        }

        [DllImport("OculusXRPlugin", CharSet=CharSet.Ansi)]
        static extern bool LoadOVRPlugin(string ovrpPath);

        [DllImport("OculusXRPlugin", CharSet=CharSet.Ansi)]
        static extern void UnloadOVRPlugin();

        [DllImport("OculusXRPlugin", CharSet=CharSet.Auto)]
        static extern void SetUserDefinedSettings(UserDefinedSettings settings);

        public OculusSettings GetSettings()
        {
            OculusSettings settings = null;
#if UNITY_EDITOR
            UnityEditor.EditorBuildSettings.TryGetConfigObject<OculusSettings>("Unity.XR.Oculus.Settings", out settings);
#else
            settings = OculusSettings.s_Settings;
#endif
            return settings;
        }

#if UNITY_EDITOR
        public string GetPreInitLibraryName(BuildTarget buildTarget, BuildTargetGroup buildTargetGroup)
        {
            return "OculusXRPlugin";
        }
#endif
    }
}
