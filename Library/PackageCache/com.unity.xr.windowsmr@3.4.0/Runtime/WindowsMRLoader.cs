using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

using UnityEngine;
using UnityEngine.XR;
using UnityEngine.XR.ARSubsystems;
using UnityEngine.XR.Management;

using XRGestureSubsystem = UnityEngine.XR.InteractionSubsystems.XRGestureSubsystem;
using XRGestureSubsystemDescriptor = UnityEngine.XR.InteractionSubsystems.XRGestureSubsystemDescriptor;

#if UNITY_INPUT_SYSTEM
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.Layouts;
using UnityEngine.InputSystem.XR;
using UnityEngine.XR.WindowsMR.Input;
#endif

#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.XR.Management;
#endif

[assembly:InternalsVisibleTo("Unity.XR.WindowsMixedReality.Editor")]
[assembly:InternalsVisibleTo("Unity.XR.WindowsMR.Tests")]

namespace UnityEngine.XR.WindowsMR
{
#if UNITY_EDITOR
    /// <summary>Generic interface used to access plugin specific settings by the loader.</summary>
    public interface IWindowsMRPackageSettings
    {
        /// <summary>Get the active build target settings.</summary>
        /// <returns>WindowsMRSettings</returns>
        WindowsMRSettings GetActiveBuildTargetSettings();
    }
#endif

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
            UnityEngine.InputSystem.InputSystem.RegisterLayout<WMRHMD>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct("(Windows Mixed Reality HMD)|(Microsoft HoloLens)|(^(WindowsMR Headset))")
            );
            UnityEngine.InputSystem.InputSystem.RegisterLayout<WMRSpatialController>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct(@"(^(Spatial Controller))|(^(OpenVR Controller\(WindowsMR))")
            );
            UnityEngine.InputSystem.InputSystem.RegisterLayout<HololensHand>(
                matches: new InputDeviceMatcher()
                    .WithInterface(XRUtilities.InterfaceMatchAnyVersion)
                    .WithProduct(@"(^(Hand -))")
            );
        }
    }
#endif

#if XR_MGMT_SUPPORTS_FILTERING
#if UNITY_EDITOR
    [XRSupportedBuildTarget(BuildTargetGroup.Standalone, new BuildTarget[]{ BuildTarget.StandaloneWindows, BuildTarget.StandaloneWindows64})]
    [XRSupportedBuildTarget(BuildTargetGroup.WSA)]
#endif
#endif
    class WindowsMRLoader : XRLoaderHelper
    {
#if UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN || UNITY_WINRT
        private static List<XRSessionSubsystemDescriptor> s_SessionSubsystemDescriptors = new List<XRSessionSubsystemDescriptor>();
        private static List<XRDisplaySubsystemDescriptor> s_DisplaySubsystemDescriptors = new List<XRDisplaySubsystemDescriptor>();
        private static List<XRInputSubsystemDescriptor> s_InputSubsystemDescriptors = new List<XRInputSubsystemDescriptor>();
        private static List<XRAnchorSubsystemDescriptor> s_AnchorSubsystemDescriptors = new List<XRAnchorSubsystemDescriptor>();
        private static List<XRMeshSubsystemDescriptor> s_MeshSubsystemDescriptors = new List<XRMeshSubsystemDescriptor>();
        private static List<XRGestureSubsystemDescriptor> s_GestureSubsystemDescriptors = new List<XRGestureSubsystemDescriptor>();

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

        public XRSessionSubsystem sessionSubsystem
        {
            get
            {
                return GetLoadedSubsystem<XRSessionSubsystem>();
            }
        }

        public XRAnchorSubsystem anchorSubsystem
        {
            get
            {
                return GetLoadedSubsystem<XRAnchorSubsystem>();
            }
        }

        public XRMeshSubsystem meshSubsystemDescriptor
        {
            get
            {
                return GetLoadedSubsystem<XRMeshSubsystem>();
            }
        }

        public XRGestureSubsystem gestureSubsystem
        {
            get
            {
                return GetLoadedSubsystem<XRGestureSubsystem>();
            }
        }

        public override bool Initialize()
        {
#if UNITY_INPUT_SYSTEM
            InputLayoutLoader.RegisterInputLayouts();
#endif
            WindowsMRSettings settings = GetSettings();
            if (settings != null)
            {
                UserDefinedSettings uds;
                uds.depthBufferType = (ushort)settings.DepthBufferFormat;
                uds.sharedDepthBuffer = (ushort)(settings.UseSharedDepthBuffer ? 1 : 0);

                SetUserDefinedSettings(uds);
            }

            CreateSubsystem<XRSessionSubsystemDescriptor, XRSessionSubsystem>(s_SessionSubsystemDescriptors, "Windows Mixed Reality Session");
            if (sessionSubsystem == null)
                return false;

            CreateSubsystem<XRDisplaySubsystemDescriptor, XRDisplaySubsystem>(s_DisplaySubsystemDescriptors, "Windows Mixed Reality Display");
            CreateSubsystem<XRInputSubsystemDescriptor, XRInputSubsystem>(s_InputSubsystemDescriptors, "Windows Mixed Reality Input");

            if (displaySubsystem == null || inputSubsystem == null)
            {
                if (displaySubsystem != null) DestroySubsystem<XRDisplaySubsystem>();
                if (inputSubsystem != null) DestroySubsystem<XRInputSubsystem>();
                DestroySubsystem<XRSessionSubsystem>();
                return false;
            }

            CreateSubsystem<XRAnchorSubsystemDescriptor, XRAnchorSubsystem>(s_AnchorSubsystemDescriptors, "Windows Mixed Reality Anchor");
            CreateSubsystem<XRMeshSubsystemDescriptor, XRMeshSubsystem>(s_MeshSubsystemDescriptors, "Windows Mixed Reality Meshing");
            CreateSubsystem<XRGestureSubsystemDescriptor, XRGestureSubsystem>(s_GestureSubsystemDescriptors, "Windows Mixed Reality Gesture");

            return base.Initialize();
        }

        public override bool Start()
        {
            StartSubsystem<XRSessionSubsystem>();
            StartSubsystem<XRDisplaySubsystem>();
            StartSubsystem<XRInputSubsystem>();
            StartSubsystem<XRAnchorSubsystem>();
            StartSubsystem<XRMeshSubsystem>();
            StartSubsystem<XRGestureSubsystem>();
            return base.Start();
        }

        public override bool Stop()
        {
            StopSubsystem<XRDisplaySubsystem>();
            StopSubsystem<XRInputSubsystem>();
            StopSubsystem<XRAnchorSubsystem>();
            StopSubsystem<XRMeshSubsystem>();
            StopSubsystem<XRGestureSubsystem>();
            StopSubsystem<XRSessionSubsystem>();
            return base.Stop();
        }

        public override bool Deinitialize()
        {
            DestroySubsystem<XRAnchorSubsystem>();
            DestroySubsystem<XRInputSubsystem>();
            DestroySubsystem<XRDisplaySubsystem>();
            DestroySubsystem<XRMeshSubsystem>();
            DestroySubsystem<XRGestureSubsystem>();
            DestroySubsystem<XRSessionSubsystem>();
            return base.Deinitialize();
        }


        [StructLayout(LayoutKind.Sequential)]
        struct UserDefinedSettings
        {
            public ushort depthBufferType;
            public ushort sharedDepthBuffer;
        }

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#else
#if ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
#endif
        static extern void SetUserDefinedSettings(UserDefinedSettings settings);

        public WindowsMRSettings GetSettings()
        {
            WindowsMRSettings settings = null;
#if UNITY_EDITOR
            UnityEngine.Object obj = null;
            UnityEditor.EditorBuildSettings.TryGetConfigObject<UnityEngine.Object>(Constants.k_SettingsKey, out obj);
            if (obj == null || !(obj is IWindowsMRPackageSettings))
                return null;

            IWindowsMRPackageSettings packageSettings = (IWindowsMRPackageSettings)obj;

            settings =  packageSettings.GetActiveBuildTargetSettings();
#else
            settings = WindowsMRSettings.s_Settings;
#endif
            return settings;
        }
#endif // UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN || UNITY_WINRT
    }

}
