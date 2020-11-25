using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

using Unity.Collections;
using Unity.Collections.LowLevel.Unsafe;
using UnityEngine.Scripting;
using UnityEngine.XR.ARSubsystems;

#if ENABLE_WINMD_SUPPORT
using Windows.Perception.Spatial;
using Windows.Storage.Streams;
#endif

namespace UnityEngine.XR.WindowsMR
{
    static class NativeApi
    {
#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern void UnityWindowsMR_refPoints_start();

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern void UnityWindowsMR_refPoints_stop();

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern void UnityWindowsMR_refPoints_onDestroy();

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern unsafe void* UnityWindowsMR_refPoints_acquireChanges(
            out void* addedPtr, out int addedCount,
            out void* updatedPtr, out int updatedCount,
            out void* removedPtr, out int removedCount,
            out int elementSize);

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern unsafe void UnityWindowsMR_refPoints_releaseChanges(void* changes);

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern bool UnityWindowsMR_refPoints_tryAdd(Pose pose, out XRAnchor anchor);

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern bool UnityWindowsMR_refPoints_tryRemove(TrackableId anchorId);

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet = CharSet.Auto)]
#endif
        public static extern void UnityWindowsMR_refPoints_ClearAllFromStorage();

#if UNITY_EDITOR
        [DllImport("Packages/com.unity.xr.windowsmr/Runtime/Plugins/x64/WindowsMRXRSDK.dll", CharSet = CharSet.Auto)]
#elif ENABLE_DOTNET
        [DllImport("WindowsMRXRSDK.dll")]
#else
        [DllImport("WindowsMRXRSDK", CharSet=CharSet.Auto)]
#endif
        public static extern void UnityWindowsMR_refPoints_ReloadStorage();
    }

    /// <summary>
    /// The WindowsMR implementation of the <c>XRAnchorSubsystem</c>. Do not create this directly.
    /// Use <c>XRAnchorSubsystemDescriptor.Create()</c> instead.
    /// </summary>
    [Preserve]
    public sealed class WindowsMRAnchorSubsystem : XRAnchorSubsystem
    {
        protected override Provider CreateProvider()
        {
            return new WindowsMRProvider();
        }

        class WindowsMRProvider : Provider
        {
            public override void Start()
            {
                NativeApi.UnityWindowsMR_refPoints_start();
            }

            public override void Stop()
            {
                NativeApi.UnityWindowsMR_refPoints_stop();
            }

            public override void Destroy()
            {
                NativeApi.UnityWindowsMR_refPoints_onDestroy();
            }

            public override unsafe TrackableChanges<XRAnchor> GetChanges(
                XRAnchor defaultAnchor,
                Allocator allocator)
            {
                int addedCount, updatedCount, removedCount, elementSize;
                void* addedPtr, updatedPtr, removedPtr;
                var context = NativeApi.UnityWindowsMR_refPoints_acquireChanges(
                    out addedPtr, out addedCount,
                    out updatedPtr, out updatedCount,
                    out removedPtr, out removedCount,
                    out elementSize);

                try
                {
                    // Yes, this is an extra copy, but the hit is small compared with the code needed to get rid of it.
                    // If this becomes a problem we can eliminate the extra copy by doing something similar to
                    // NativeCopyUtility.PtrToNativeArrayWithDefault only with a pre-allocated array properties
                    // from using the TrackableChanges(int, int, int allocator) constructor.
                    var added = NativeCopyUtility.PtrToNativeArrayWithDefault<XRAnchor>(defaultAnchor, addedPtr, elementSize, addedCount, allocator);
                    var updated = NativeCopyUtility.PtrToNativeArrayWithDefault<XRAnchor>(defaultAnchor, updatedPtr, elementSize, updatedCount, allocator);
                    var removed = NativeCopyUtility.PtrToNativeArrayWithDefault<TrackableId>(default(TrackableId), removedPtr, elementSize, removedCount, allocator);


                    var ret = TrackableChanges<XRAnchor>.CopyFrom(
                        added,
                        updated,
                        removed,
                        allocator);

                    added.Dispose();
                    updated.Dispose();
                    removed.Dispose();
                    return ret;

                }
                finally
                {
                    NativeApi.UnityWindowsMR_refPoints_releaseChanges(context);
                }
            }

            public override bool TryAddAnchor(
                Pose pose,
                out XRAnchor anchor)
            {
                return NativeApi.UnityWindowsMR_refPoints_tryAdd(pose, out anchor);
            }

            public override bool TryRemoveAnchor(TrackableId anchorId)
            {
                return NativeApi.UnityWindowsMR_refPoints_tryRemove(anchorId);
            }
        }

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.SubsystemRegistration)]
        static void RegisterDescriptor()
        {
            XRAnchorSubsystemDescriptor.Create(new XRAnchorSubsystemDescriptor.Cinfo
            {
                id = "Windows Mixed Reality Anchor",
                subsystemImplementationType = typeof(WindowsMRAnchorSubsystem),
                supportsTrackableAttachments = false
            });
        }
    }

#if ENABLED_FOR_MSFT // Disabled at request from Mirosoft, but leaving the code in so we can re-enable or change if necessary.
    public static class WMRRPExtensions
    {
        public static void ClearAllAnchorsFromStorage(this WindowsMRAnchorSubsystem wmrrp)
        {
            NativeApi.UnityWindowsMR_refPoints_ClearAllFromStorage();
        }

        public static void ReloadStorage(this WindowsMRAnchorSubsystem wmrrp)
        {
            NativeApi.UnityWindowsMR_refPoints_ReloadStorage();
        }

#pragma  warning disable CS0618
#pragma  warning disable CS1998
        public static async Task<bool> ImportAnchors(this WindowsMRAnchorSubsystem wmrrp, Stream input)
        {
            bool ret = false;
#if ENABLE_WINMD_SUPPORT

            try
            {
                var access = await SpatialAnchorTransferManager.RequestAccessAsync();
                if (access != SpatialPerceptionAccessStatus.Allowed)
                    return ret;

                var spatialAnchorStore = await SpatialAnchorManager.RequestStoreAsync();
                if (spatialAnchorStore == null)
                    return ret;

                var anchors = await SpatialAnchorTransferManager.TryImportAnchorsAsync(input.AsInputStream());
                foreach (var kvp in anchors)
                {
                    spatialAnchorStore.TrySave(kvp.Key, kvp.Value);
                }

                ret = true;
            }
            catch (Exception e)
            {
                Debug.Log(e);
                ret = false;
            }
#else
            Debug.LogError("This API is only available for use in UWP based applications.");
#endif //ENABLE_WINMD_SUPPORT
            return ret;
        }

        public static async Task<bool> ExportAnchors(this WindowsMRAnchorSubsystem wmrrp, Stream output)
        {
            bool ret = false;

#if ENABLE_WINMD_SUPPORT
            try
            {
                Debug.Log($"Getting Spatial Anchor Transfer Manager Access");
                var access = await SpatialAnchorTransferManager.RequestAccessAsync();
                if (access != SpatialPerceptionAccessStatus.Allowed)
                {
                    Debug.Log($"Access check failed with {access}");
                    return ret;
                }

                Debug.Log($"Getting Spatial Anchor Store");
                var spatialAnchorStore = await SpatialAnchorManager.RequestStoreAsync();
                if (spatialAnchorStore == null)
                    return ret;

                Debug.Log($"Setting up stream");
                var stream = output.AsOutputStream();

                Debug.Log($"Getting saved anchors");
                var anchors = spatialAnchorStore.GetAllSavedAnchors();
                if (anchors == null || anchors.Count == 0)
                {
                    Debug.Log("No anchors to exort!!!");
                }
                else
                {
                    Debug.Log("Exporting anchors...");
                    ret = await SpatialAnchorTransferManager.TryExportAnchorsAsync(anchors, stream);
                    Debug.Log(ret ? "SUCCESS" : "FAILURE");
                }
            }
            catch (Exception e)
            {
                Debug.Log(e);
                ret = false;
            }
#else
            Debug.LogError("This API is only available for use in UWP based applications.");
#endif //ENABLE_WINMD_SUPPORT
            return ret;
        }
#pragma  warning restore CS1998
#pragma  warning restore CS0618
    }
#endif //ENABLED_FOR_MSFT
}

