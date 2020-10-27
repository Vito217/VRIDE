using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.Events;
using System.Runtime.InteropServices;

namespace Vive.Plugin.SR
{
    public class ViveSR : MonoBehaviour
    {
        public class FrameworkInitialError {
            public int ErrorCode = (int)Error.WORK;
            public ModuleType FailedModule = 0;
        }
        /// <summary>
        /// Status of SRanipal context and engines.
        /// </summary>
        public static FrameworkStatus FrameworkStatus { get; protected set; }
        public static FrameworkInitialError InitialError = new FrameworkInitialError();
        /// <summary>
        /// Enable the passthrough engine or not.
        /// </summary>
        [Header("[ViveSR Framework Pre-Setting]")]
        public bool InitializePassThroughModule;
        public bool InitializeDepthModule;
        public bool InitializeDepthMeshModule;
        public bool InitializeRigidReconstructionModule;
        public bool InitializeAISceneModule;

        [Header("[ViveSR Framework PassThrough Data Setting]")]
        public bool InitializeNon4KDistortedPassThroughData;

        public static bool UpdateUnityPassThrough = false;
        public static bool UpdateUnityNon4KDistortedPassThroughData = false;
        public static bool UpdateUnityDepthMesh = false;
        public static bool UpdateUnityReconstruction = false;
        public static bool UpdateUnityAIScene = false;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable EnablePassThroughModule has changed to InitializePassThroughModule.
        * @warning The variable will remove in the future.
        */
        public bool EnablePassThroughModule
        {
            get { return InitializePassThroughModule; }
            set { InitializePassThroughModule = value; }
        }
        /**
        * The variable EnableDepthModule has changed to InitializeDepthModule. 
        * @warning The variable will remove in the future.
        */
        public bool EnableDepthModule
        {
            get { return InitializeDepthModule; }
            set { InitializeDepthModule = value; }
        }
        /**
        * The variable EnableDepthMeshModule has changed to InitializeDepthMeshModule. 
        * @warning The variable will remove in the future.
        */
        public bool EnableDepthMeshModule
        {
            get { return InitializeDepthMeshModule; }
            set { InitializeDepthMeshModule = value; }
        }
        /**
        * The variable EnableRigidReconstructionModule has changed to InitializeRigidReconstructionModule.
        * @warning The variable will remove in the future.
        */
        public bool EnableRigidReconstructionModule
        {
            get { return InitializeRigidReconstructionModule; }
            set { InitializeRigidReconstructionModule = value; }
        }
        /**        
        * The variable EnablePassThroughNon4KDistortDataUse has changed to InitializeNon4KDistortedPassThroughData.
        * @warning The variable will remove in the future.
        */
        public bool EnablePassThroughNon4KDistortDataUse
        {
            get { return InitializeNon4KDistortedPassThroughData; }
            set { InitializeNon4KDistortedPassThroughData = value; }
        }
        /**
        * The variable EnableUnityPassThrough has changed to UpdateUnityPassThrough.
        * @warning The variable will remove in the future.
        */
        public static bool EnableUnityPassThrough
        {
            get { return UpdateUnityPassThrough; }
            set { UpdateUnityPassThrough = value; }
        }
        /**
        * The variable EnableUnityPassThroughNon4KDistortData has changed to UpdateUnityNon4KDistortedPassThroughData.
        * @warning The variable will remove in the future.
        */
        public static bool EnableUnityPassThroughNon4KDistortData
        {
            get { return UpdateUnityNon4KDistortedPassThroughData; }
            set { UpdateUnityNon4KDistortedPassThroughData = value; }
        }
        /**
        * The variable EnableUnityDepthMesh has changed to UpdateUnityDepthMesh.
        * @warning The variable will remove in the future.
        */
        public static bool EnableUnityDepthMesh
        {
            get { return UpdateUnityDepthMesh; }
            set { UpdateUnityDepthMesh = value; }
        }
        /**
        * The variable EnableUnityReconstruction has changed to UpdateUnityReconstruction.
        * @warning The variable will remove in the future.
        */
        public static bool EnableUnityReconstruction
        {
            get { return UpdateUnityReconstruction; }
            set { UpdateUnityReconstruction = value; }
        }
        /**
        * The variable EnableUnityAI has changed to UpdateUnityAIScene.
        * @warning The variable will remove in the future.
        */
        public static bool EnableUnityAI
        {
            get { return UpdateUnityAIScene; }
            set { UpdateUnityAIScene = value; }
        }
        #endregion

        [Header("[ViveSR Modules Unity Registration]")]
        public ViveSR_Module[] Modules = new ViveSR_Module[3];

        private List<ModuleType> ModuleTypes = new List<ModuleType>();

        /// <summary>
        /// Event for the SRWorks runtime status.
        /// </summary>
        public static ViveSR_RuntimeStatusEvent RuntimeStatusEvent = new ViveSR_RuntimeStatusEvent();

        private static int PreviousRuntimeStatus = 0;

        private static ViveSR Mgr = null;

        private bool RefinementSetting = false;

        public static ViveSR Instance
        {
            get
            {
                if (Mgr == null)
                {
                    Mgr = FindObjectOfType<ViveSR>();
                }
                if (Mgr == null)
                {
                    Debug.LogError("SRWork_Module_Framework does not be attached on GameObject");
                }
                return Mgr;
            }
        }

        // Use this for initialization
        void Start()
        {
            StartFramework();
        }

        void Update()
        {
            UpdateWhileWorking();

            if (UpdateUnityPassThrough)
                PassThrough.SRWork_PassThrough.UpdateData();

            // UpdateData will be called in ExtractMeshDataThread
            // Also we don't need to get depth data
            if (ViveSR_DualCameraImageRenderer.UpdateDepthMaterial)
            {
                Depth.SRWork_Depth.UpdateData();
            }
            if (UpdateUnityDepthMesh)
            {
                if (DepthMesh.SRWork_Depth_Mesh.UpdateData()) {
                    ((ViveSR_DualCameraDepthCollider)Modules[1]).ExtractMeshData();
                }
            }
            if (UpdateUnityReconstruction
                && !ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo
                && !ViveSR_RigidReconstruction.IsExporting)
            {
                if (RigidReconstruction.SRWork_Rigid_Reconstruciton.UpdateData()) {
                    ((ViveSR_RigidReconstructionRenderer)Modules[2]).ExtractMeshData();
                }
            }
            if (ViveSR_RigidReconstruction.IsExporting) {
                ViveSR_RigidReconstruction.UpdateExportProgress();
            }
            if (ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo) {
                ViveSR_SceneUnderstanding.UpdateSceneUnderstandingProgress();
            }
            if (UpdateUnityAIScene && ViveSR_AIScene.IsAISceneProcessing) {
                AIScene.SRWork_AI_Scene.UpdateData();
            }
        }
        void Release()
        {
            if (UpdateUnityPassThrough == true)
                Modules[0].Release();
            if (UpdateUnityDepthMesh == true)
                Modules[1].Release();
            if (UpdateUnityReconstruction == true)
                Modules[2].Release();
            ViveSR_DualCameraDepthCollider.UpdateDepthCollider = false;
        }
        void OnApplicationQuit()
        {
            Release();
            StopFramework();
            SRWorkModule_API.StopViveSR();
        }

        private delegate void RuntimeStatusCallback(int status);
        public static void RuntimeStatusHandler(int status)
        {
            // Use bitwise XOR to compute which status flag changed.
            int statusDifference = status ^ PreviousRuntimeStatus;

            // Convert the status difference to a list of changed flags.
            List<int> changedFlags;
            ConvertIntToBits(statusDifference, out changedFlags);
            foreach (var flag in changedFlags)
            {
                // Get the flag value: the bit value of `status` at position `flag`.
                var flagValue = ((status >> flag) & 0x00000001) == 0x00000001;
                // Invoke a corresponding event.
                RuntimeStatusEvent.Invoke((RuntimeStatusFlag)flag, flagValue);
            }

            // Store the status.
            PreviousRuntimeStatus = status;
        }

        public void StartFramework()
        {
            if (FrameworkStatus == FrameworkStatus.WORKING) return;
            FrameworkStatus = FrameworkStatus.START;

            ModuleTypes.Clear();
            if (InitializePassThroughModule)
            {
                ModuleTypes.Add(ModuleType.PASSTHROUGH);
                ModuleTypes.Add(ModuleType.PASSTHROUGH4K);
                ModuleTypes.Add(ModuleType.CONTROLLER_POSE);
            }
            if (InitializeDepthModule) ModuleTypes.Add(ModuleType.DEPTH);
            if (InitializeDepthMeshModule) ModuleTypes.Add(ModuleType.DEPTHMESH);
            if (InitializeRigidReconstructionModule) ModuleTypes.Add(ModuleType.RIGIDRECONSTRUCTION);
            if (InitializeAISceneModule) ModuleTypes.Add(ModuleType.AI_Scene);

            InitialModule();
            if (InitializeRigidReconstructionModule)
            {
                ModuleStatus recons_status = ModuleStatus.WORKING;
                SRWorkModule_API.GetStatus(ModuleType.RIGIDRECONSTRUCTION, out recons_status);
                if (ModuleStatus.BLOCKED == recons_status)
                    InitializeRigidReconstructionModule = false;
            }
            if (InitializeAISceneModule)
            {
                ModuleStatus aiscene_status = ModuleStatus.WORKING;
                SRWorkModule_API.GetStatus(ModuleType.AI_Scene, out aiscene_status);
                if (ModuleStatus.BLOCKED == aiscene_status)
                    InitializeAISceneModule = false;
            }
            SRWorkModule_API.SetRuntimeStatusCallback(Marshal.GetFunctionPointerForDelegate((RuntimeStatusCallback)RuntimeStatusHandler));
        }
        public void StopFramework()
        {
            if (FrameworkStatus != FrameworkStatus.STOP)
            {
                foreach (var type in ModuleTypes)
                {
                    int result = SRWorkModule_API.Release(type);
                    if (result == (int)Error.WORK) Debug.Log("[SRWorkModule] Release " + type + " : " + result);
                    else Debug.LogWarning("[SRWorkModule] Release " + type + " : " + result);
                }
            }
            else
            {
                Debug.Log("[SRWorkModule] Stop Framework : not open");
            }
            FrameworkStatus = FrameworkStatus.STOP;
        }
        private void InitialModule()
        {
            foreach (var type in ModuleTypes)
            {
                InitialError.ErrorCode = SRWorkModule_API.Initial(type, IntPtr.Zero);
                if (InitialError.ErrorCode != (int)Error.WORK) {
                    InitialError.FailedModule = type;
                    Debug.LogWarning("[SRWorkModule] Initial " + type + " : " + InitialError.ErrorCode);
                    if (InitialError.ErrorCode == (int)Error.EULA_NOT_ACCEPT)
                    {
#if UNITY_EDITOR
                        UnityEditor.EditorApplication.isPlaying = false;
#else
                        RuntimeStatusEvent.Invoke(RuntimeStatusFlag.INITIIAL_FAIL, false);
#endif
                    }
                    FrameworkStatus = FrameworkStatus.ERROR;
                    return;
                }
                Debug.Log("[SRWorkModule] Initial " + type + " : " + InitialError.ErrorCode);
            }
            FrameworkStatus = FrameworkStatus.WORKING;
        }
        private void UpdateWhileWorking()
        {
            if (FrameworkStatus != FrameworkStatus.WORKING)
                return;

            if (InitializePassThroughModule == true && UpdateUnityPassThrough == false)
            {
                Modules[0].Initial();
                SRWorkModule_API.TurnOffUndistortDataToDepth();
                SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.DEPTH);
                if (PassThrough.SRWork_PassThrough.Image4kReady)
                {
                    PassThrough.SRWork_PassThrough.SkipVGAPassThrough(true);
                }
                UpdateUnityPassThrough = true;
            }

            if (InitializeNon4KDistortedPassThroughData == true && UpdateUnityNon4KDistortedPassThroughData == false)
            {
                bool result = PassThrough.SRWork_PassThrough.TurnOnPassThroughDistortData();
                if (result) UpdateUnityNon4KDistortedPassThroughData = true;
            }
            else if (InitializeNon4KDistortedPassThroughData == false && UpdateUnityNon4KDistortedPassThroughData == true)
            {
                bool result = PassThrough.SRWork_PassThrough.TurnOffPassThroughDistortData();
                if (result) UpdateUnityNon4KDistortedPassThroughData = false;
            }

            if ((InitializeDepthMeshModule == true || InitializeDepthModule == true)
                && UpdateUnityDepthMesh == false)
            {
                Modules[1].Initial();
                //Get refinement setting of engine.
                int result = SRWorkModule_API.GetDepthParameterBool((int)DepthCmd.ENABLE_REFINEMENT, ref RefinementSetting);
                if (result == (int)SR.Error.WORK)
                    ViveSR_DualCameraImageCapture.IsDepthRefinementEnabled = RefinementSetting;
                SRWorkModule_API.UnlinkModule((int)ModuleType.DEPTH, (int)ModuleType.DEPTHMESH);
                UpdateUnityDepthMesh = true;
            }

            if (InitializeRigidReconstructionModule == true && UpdateUnityReconstruction == false)
            {
                Modules[2].Initial();
                SRWorkModule_API.UnlinkModule((int)ModuleType.DEPTH, (int)ModuleType.RIGIDRECONSTRUCTION);
                UpdateUnityReconstruction = true;
            }
            if (InitializePassThroughModule == true && InitializeAISceneModule == true && UpdateUnityAIScene == false && InitializeDepthModule==true)
            {
                if (PassThrough.SRWork_PassThrough.Image4kReady)
                    SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH4K, (int)ModuleType.AI_Scene);
                else
                    SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.AI_Scene);
                UpdateUnityAIScene = true;
            }
        }

        /// <summary>
        /// Convert an integer to a list of bit positions where the bit value is one.
        /// </summary>
        public static void ConvertIntToBits(int number, out List<int> oneBitPositions)
        {
            oneBitPositions = new List<int>();
            const int numBits = sizeof(int) * 8;
            for (int i = 0; i < numBits; ++i)
            {
                if ((number & 0x00000001) == 0x00000001)
                {
                    // If the bit is one, output the bit position.
                    oneBitPositions.Add(i);
                }
                // Shift one bit to the right.
                number = number >> 1;
            }
        }
    }
}
