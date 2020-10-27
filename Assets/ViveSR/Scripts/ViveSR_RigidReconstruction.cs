using UnityEngine;
using System.Runtime.InteropServices;
using System;
using Vive.Plugin.SR.RigidReconstruction;

namespace Vive.Plugin.SR
{
    public class ViveSR_RigidReconstruction
    {
        public static string ReconsMeshLayerName = "Default";

        [Obsolete("use ViveSR_Framework.CallbackBasic instead.")]
        public delegate void Callback(int numFrame, IntPtr poseMtx, IntPtr vertData, int numVert, int vertStide, IntPtr idxData, int numIdx);
        public delegate void ExportProgressCallback(int stage, int percentage);

        //private static DataInfo[] DataInfoPointCloud = null;
        private static bool InitialPointCloudPtrSize = false;

        private static int[] RawPointCloudFrameIndex = new int[1];
        private static int[] RawPointCloudVerticeNum = new int[1];
        private static int[] RawPointCloudIndicesNum = new int[1];
        private static int[] RawPointCloudBytePerVetex = new int[1];
        private static int[] RawPointCloudSectorNum = new int[1];
        private static int[] RawModelChunkNum = new int[1];
        private static int[] RawModelChunkIdx = new int[1];

        private static float[] OutVertex;
        private static int[] OutIndex;
        private static float[] TrackedPose;
        private static int ExportStage;
        private static int ExportPercentage;
        private static int ExportError;
        private static int ScannedMeshPreview;
        private static int FrameSeq { get { return RawPointCloudFrameIndex[0]; } }
        private static int VertNum { get { return RawPointCloudVerticeNum[0]; } }
        private static int IdxNum { get { return RawPointCloudIndicesNum[0]; } }
        private static int VertStrideInByte { get { return RawPointCloudBytePerVetex[0]; } }
        private static int SectorNum { get { return RawPointCloudSectorNum[0]; } }
        private static int[] SectorIDList;
        private static int[] SectorVertNum;
        private static int[] SectorMeshIdNum;
        private static int ModelChunkNum { get { return RawModelChunkNum[0]; } }
        private static int ModelChunkIdx { get { return RawModelChunkIdx[0]; } }
        private static bool UsingCallback = false;

        private static int PreviousQuality = 1;
        private static float GeometrySize = 0.02f; // default: MID quality
        private static float ColorSize = 0.02f; // default: MID quality

        public static bool ExportAdaptiveMesh { get; set; }
        public static float ExportAdaptiveMaxGridSize { get; set; }
        public static float ExportAdaptiveMinGridSize { get; set; }
        public static float ExportAdaptiveErrorThres { get; set; }
        public static float LiveAdaptiveMaxGridSize { get; set; }
        public static float LiveAdaptiveMinGridSize { get; set; }
        public static float LiveAdaptiveErrorThres { get; set; }
        public static bool IsScanning { get; private set; }
        public static bool IsExporting { get; private set; }
        public static bool IsDuringScannedMeshPreview { get; private set; }
        public static bool IsScannedMeshPreviewCompleted { get; private set; }
        public static bool ReconstructionProcessing { get; private set; }
        private static DepthCase DepthCase = DepthCase.DEFAULT;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable IsExportingMesh has changed to IsExporting.
        * @warning The variable will remove in the future.
        */
        public static bool IsExportingMesh
        {
            get { return IsExporting; }
            private set { }
        }
        /**
        * The function ExportModel has changed to StartExporting.
        * @warning The function will remove in the future.
        */
        public static void ExportModel(string filename)
        {
            ExportStage = 0;
            ExportPercentage = 0;
            ExportError = (int)Error.WORK;
            IsExporting = true;
            IsScannedMeshPreviewCompleted = false;

            SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.EXPORT_ADAPTIVE_MODEL, ExportAdaptiveMesh);
            if (ExportAdaptiveMesh)
            {
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MAX_GRID, ExportAdaptiveMaxGridSize * 0.01f);   // cm to m
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MIN_GRID, ExportAdaptiveMinGridSize * 0.01f);
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_ERROR_THRES, ExportAdaptiveErrorThres);
            }

            byte[] bytearray = System.Text.Encoding.ASCII.GetBytes(filename);
            IntPtr parameter = Marshal.AllocCoTaskMem(filename.Length);
            Marshal.Copy(bytearray, 0, parameter, filename.Length);

            SRWorkModule_API.SetReconstructionOutputFileName(parameter, filename.Length);
        }
        #endregion
        public static int EnableReconstructionProcess(bool active)
        {
            int result = (int)Error.FAILED;
            if (active && !ReconstructionProcessing)
            {
                result = StartReconstructionModule();
                if (result == (int)Error.WORK) ReconstructionProcessing = true;
                else ReconstructionProcessing = false;
            }
            else if (!active && ReconstructionProcessing)
            {
                result = StopReconstructionModule();
                ReconstructionProcessing = false;
            }
            return result;
        }


        public static int StartReconstructionModule() {
            // Store the current depth case.
            DepthCase = ViveSR_DualCameraImageCapture.DepthCase;

            SRWorkModule_API.SetSkipVGAProcess(false);
            SRWorkModule_API.SetDepthParameterBool((int)DepthCmd.CHANGE_DEPTH_CASE_2_RECONSTRUCT_USED, true);
            return SRWorkModule_API.LinkModule((int)ModuleType.DEPTH,(int)ModuleType.RIGIDRECONSTRUCTION);
        }

        public static int StopReconstructionModule()
        {
            SRWorkModule_API.SetDepthParameterBool((int)DepthCmd.CHANGE_DEPTH_CASE_2_RECONSTRUCT_USED, false);

            // Restore the original depth case.
            ViveSR_DualCameraImageCapture.SetDepthCase(DepthCase);

            return SRWorkModule_API.UnlinkModule((int)ModuleType.DEPTH, (int)ModuleType.RIGIDRECONSTRUCTION);
        }

        public static bool InitRigidReconstructionParamFromFile(string config_file)
        {
            //return ViveSR_Framework.SetParameterString(ViveSR_Framework.MODULE_ID_RIGID_RECONSTRUCTION, (int)ReconstructionParam.CONFIG_FILEPATH, configFile) == (int)Error.WORK;
            return false;
        }

        public static int GetRigidReconstructionIntParameter(int type)
        {
            int ret = -1;

            if (SRWorkModule_API.GetReconstructionParameterInt(type, ref ret) != (int)Error.WORK)
                Debug.Log("[ViveSR] [RigidReconstruction] GetRigidReconstructionIntParameter Failed");

            return ret;
        }

        public static void AllocOutputDataMemory()
        {
            InitialPointCloudPtrSize = false;
            OutVertex = new float[8 * 2500000];
            OutIndex = new int[2500000];
            TrackedPose = new float[16];
            //ExternalPose = new float[16];
            SectorIDList = new int[1000000];
            SectorVertNum = new int[1000000];
            SectorMeshIdNum = new int[1000000];

            Debug.Log("[ViveSR] [RigidReconstruction] AllocOutputMemory Done");

            ExportAdaptiveMesh = true;
            LiveAdaptiveMaxGridSize = ExportAdaptiveMaxGridSize = 64;
            LiveAdaptiveMinGridSize = ExportAdaptiveMinGridSize = 4;
            LiveAdaptiveErrorThres  = ExportAdaptiveErrorThres = 0.4f;            
        }

        public static void ReleaseAllocOutputDataMemory()
        {
            OutVertex = null;
            OutIndex = null;
            TrackedPose = null;
            SectorIDList = null;
            SectorVertNum = null;
            SectorMeshIdNum = null;
        }

        public static bool GetRigidReconstructionFrame(ref int frame)
        {
            if (!UsingCallback)
            {
                RawPointCloudFrameIndex[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.frame_seq;
            }
            frame = RawPointCloudFrameIndex[0];
            return true;
        }

        // live data
        public static bool GetRigidReconstructionData(ref int frame, 
                                                      out float[] pose, 
                                                      ref int vertices_num, 
                                                      out float[] vertices_buff, 
                                                      ref int vert_strideIn_float, 
                                                      out int[] sector_id_list, 
                                                      ref int sector_mum, 
                                                      out int[] sector_ver_num, 
                                                      out int[] sector_mesh_Id_num,
                                                      ref int indices_num, 
                                                      out int[] indices_buff)
        {
            if (!UsingCallback)
            {
                int result = (int)Error.WORK;
                if (!InitialPointCloudPtrSize)
                {
                    InitialPointCloudPtrSize = (result == (int)Error.WORK);
                }
                if (result == (int)Error.WORK)
                {
                    ParseReconstructionPtrData();
                }
            }
             
            bool isUpdated = (vertices_num != VertNum);

            vertices_num = VertNum;
            indices_num = IdxNum;
            frame = FrameSeq;
            vert_strideIn_float = VertStrideInByte / 4;
            vertices_buff = OutVertex;
            indices_buff = OutIndex;
            pose = TrackedPose;
            sector_id_list = SectorIDList;
            sector_mum = SectorNum;
            sector_ver_num = SectorVertNum;
            sector_mesh_Id_num = SectorMeshIdNum;
            return isUpdated;
        }

        // get model data chunk by chunk
        public static bool GetScannedModelPreviewData(ref int model_chunk_num,
                                                                ref int model_chunk_idx,
                                                                ref int model_vert_num,
                                                                out float[] model_vertices,
                                                                ref int model_idx_num,
                                                                out int[] model_indices)
        {
            if (!UsingCallback)
            {
                ParseReconstructionPtrData();
            }

            bool isUpdated = (ModelChunkNum > 0 && model_chunk_idx != ModelChunkIdx);

            model_chunk_num = ModelChunkNum;
            model_chunk_idx = ModelChunkIdx;
            model_vert_num = VertNum;
            model_vertices = OutVertex;
            model_idx_num = IdxNum;
            model_indices = OutIndex;

            return isUpdated;
        }

        public static int RegisterReconstructionCallback()
        {
            return 1;
        }

        public static int UnregisterReconstructionCallback()
        {
            return 1;
        }

        private static void ReconstructionDataCallback(int key)
        {
        }

        private static void ParseReconstructionPtrData()
        {
            RawPointCloudFrameIndex[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.frame_seq;
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.posemtx44, TrackedPose, 0, TrackedPose.Length);
            RawPointCloudVerticeNum[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.num_vertices;
            RawPointCloudBytePerVetex[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.bytepervert;
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.vertices, OutVertex, 0, (VertNum * VertStrideInByte / sizeof(float)));
            RawPointCloudIndicesNum[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.num_indices;
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.indices, OutIndex, 0, IdxNum /**sizeof(int)*/);
            RawPointCloudSectorNum[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.sector_num;
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.sector_id_list, SectorIDList, 0, SectorNum);
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.sector_vert_num, SectorVertNum, 0, SectorNum);
            Marshal.Copy(SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.sector_idx_num, SectorMeshIdNum, 0, SectorNum);
            RawModelChunkNum[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.model_chunk_num;
            RawModelChunkIdx[0] = SRWork_Rigid_Reconstruciton.rigid_reconstruction_data_.model_chunk_idx;
        }

        public static void StartExporting(string filename)
        {
            if (ViveSR_DualCameraImageCapture.IsDepthProcessing)
                ViveSR_DualCameraImageCapture.EnableDepthProcess(false);

            ExportStage = 0;
            ExportPercentage = 0;
            ExportError = (int)Error.WORK;
            IsExporting = true;
            IsScannedMeshPreviewCompleted = false;

            SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.EXPORT_ADAPTIVE_MODEL, ExportAdaptiveMesh);
            if (ExportAdaptiveMesh)
            {
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MAX_GRID, ExportAdaptiveMaxGridSize * 0.01f);   // cm to m
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MIN_GRID, ExportAdaptiveMinGridSize * 0.01f);
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_ERROR_THRES, ExportAdaptiveErrorThres);
            }

            byte[] bytearray = System.Text.Encoding.ASCII.GetBytes(filename);
            IntPtr parameter = Marshal.AllocCoTaskMem(filename.Length);
            Marshal.Copy(bytearray, 0, parameter, filename.Length);

            SRWorkModule_API.SetReconstructionOutputFileName(parameter, filename.Length);
        }
        public static void UpdateExportProgress()
        {
            ExportError = SRWorkModule_API.GetExportMeshProgress(ref ExportPercentage);
            if (ExportError != (int)Error.WORK || ExportPercentage == 100)
            {
                if (!ViveSR_DualCameraImageCapture.IsDepthProcessing)
                    ViveSR_DualCameraImageCapture.EnableDepthProcess(true);

                IsExporting = false;
            }
        }

        public static void ExtractModelPreviewData()
        {
            ExportStage = 0;
            ScannedMeshPreview = 0;
            IsDuringScannedMeshPreview = true;

            SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.EXPORT_ADAPTIVE_MODEL, ExportAdaptiveMesh);
            if (ExportAdaptiveMesh)
            {
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MAX_GRID, ExportAdaptiveMaxGridSize * 0.01f);   // cm to m
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MIN_GRID, ExportAdaptiveMinGridSize * 0.01f);
                SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_ERROR_THRES, ExportAdaptiveErrorThres);
            }
            SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.MODEL_PREVIEW_START_FOR_UNITY), true);
        }

        //Current runtime doesn't have callback.
        private static void UpdateModelPreviewProgress(int stage, int percentage)
        {
            if (stage == (int)ReconstructionExportStage.STAGE_EXTRACTING_MODEL) ExportStage = 0;
            else if (stage == (int)ReconstructionExportStage.STAGE_COMPACTING_TEXTURE) ExportStage = 1;
            else if (stage == (int)ReconstructionExportStage.STAGE_SAVING_MODEL_FILE) ExportStage = 2;
            else if (stage == (int)ReconstructionExportStage.STAGE_EXTRACTING_COLLIDER) ExportStage = 3;
            ScannedMeshPreview = percentage;

            if (ExportStage == 0 && ScannedMeshPreview == 100)
            {
                StopScanning();
                CompleteModelPreview();
                Debug.Log("[ViveSR] [RigidReconstruction] Complete Scanned Model Preview");
            }
        }

        public static int GetExportProgress(ref int percentage)
        {
            //percentage = ExportStage * 25 + (int)(ExportPercentage * 0.25f);
            percentage = ExportPercentage;
            return ExportError;
        }

        public static void EnableLiveMeshExtraction(bool enable)
        {
            SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.EXTRACT_POINT_CLOUD), enable);
        }

        public static void SetLiveMeshExtractionMode(ReconstructionLiveMeshExtractMode mode)
        {
            SRWorkModule_API.SetReconstructionParameterInt((int)(ReconstructionCmd.EXTRACT_VERTEX_NORMAL), (int)mode);
        }

        public static void StartScanning()
        {
            ViveSR_RigidReconstruction.EnableReconstructionProcess(true);
            if (ReconstructionProcessing)
            {
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.START), true);
                IsScanning = true;
                IsScannedMeshPreviewCompleted = false;
                Debug.Log("start");
            }
        }

        public static void StopScanning()
        {
            if (ReconstructionProcessing)
            {
                IsScanning = false;
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.STOP), true);
                Debug.Log("stop");
            }
            ViveSR_RigidReconstruction.EnableReconstructionProcess(false);

        }

        public static void CompleteModelPreview()
        {
            IsScannedMeshPreviewCompleted = true;
            IsDuringScannedMeshPreview = false;
        }

        public static int ResetReconstructionModule()
        {
            return SRWorkModule_API.ResetReconstructionModule();
        }
		
		public static int TerminateExporting()
        {
            int err = SRWorkModule_API.TerminateExporting();
            IsExporting = false;
            return err;
        }


        public static void RegisterDataErrorHandler(int error_code, Action callback)
        {
            RigidReconstruction.SRWork_Rigid_Reconstruciton.DataErrorEvent.RegisterHandler(error_code, callback);
        }

        public static void UnregisterDataErrorHandler(int error_code)
        {
            RigidReconstruction.SRWork_Rigid_Reconstruciton.DataErrorEvent.UnregisterHandler(error_code);
        }

        public static void SetReconstructionUpdateQuality(int SelectedQuality)
        {
            if (PreviousQuality == SelectedQuality)
                return;
            switch (SelectedQuality)
            {
                case 0:
                    ViveSR_RigidReconstruction.SetReconstructionGeometrySize(0.04f);
                    ViveSR_RigidReconstruction.SetReconstructionColorSize(0.04f);
                    break;
                case 1:
                    ViveSR_RigidReconstruction.SetReconstructionGeometrySize(0.02f);
                    ViveSR_RigidReconstruction.SetReconstructionColorSize(0.02f);
                    break;
                case 2:
                    ViveSR_RigidReconstruction.SetReconstructionGeometrySize(0.01f);
                    ViveSR_RigidReconstruction.SetReconstructionColorSize(0.01f);
                    break;
            }
            PreviousQuality = SelectedQuality;
        }

        public static int SetReconstructionGeometrySize(float geometrySize)
        {
            GeometrySize = geometrySize;
            return SRWorkModule_API.SetReconstructionGeometrySize(GeometrySize);
        }
        public static int SetReconstructionColorSize(float colorSize)
        {
            ColorSize = colorSize;
            return SRWorkModule_API.SetReconstructionColorSize(ColorSize);
        }
        public static float GetReconstructionGeometrySize() {
            SRWorkModule_API.GetReconstructionGeometrySize(ref GeometrySize);
            return GeometrySize;
        }
        public static float GetReconstructionColorSize() {
            SRWorkModule_API.GetReconstructionColorSize(ref ColorSize);
            return ColorSize;
        }

    }

}
