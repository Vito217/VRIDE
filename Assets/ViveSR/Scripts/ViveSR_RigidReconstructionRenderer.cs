using System;
using UnityEngine;
using System.Linq;
using System.Threading;
using System.Collections.Generic;
using System.Collections;

namespace Vive.Plugin.SR
{
    public class ViveSR_RigidReconstructionRenderer : ViveSR_Module
    {
        [Header("Init Config")]
        public string ConfigFilePath = "";        

        [Header("Rendering Control")]
        public ReconstructionQuality FullScenePointQuality = ReconstructionQuality.MID;
        [Range(300, 1000)]
        public int RefreshIntervalMS = 300;
        
        public static ReconstructionDisplayMode LiveMeshDisplayMode { get; set; }
        public static bool LiveMeshCollider = false;
        private static bool PrevLiveMeshCollider = false;
        private int LastLiveMeshDisplayMode = 10;   // UN-DEFINED
        private static int ThreadPeriod = 15;

        public static bool EnableSector = true;
        public static int MaxActiveGO = 200;
        private bool LastEnableSector = false;
        private bool BackupSectorValue = true;
        private float SectorSizeInMeter = 0.8f;
        public static bool SetWireFrameOpaque = true;
        private bool LastSetWireFrameOpaque = true;
        public static float SetWireFrameThickness = 100.0f;
        private float LastSetWireFrameThickness = 100.0f;

        [SerializeField]
        private Material LiveMeshMaterial;
        [SerializeField]
        private Material WireframeMaterial;

        [Header("Information")]
        [ReadOnly] public int VertexCount;
        [ReadOnly] public int IndexCount;
        [ReadOnly] public int CurrentFrameID;
        [ReadOnly] public int VertStrideInFloat;
        [ReadOnly] public int ColliderCount;
        [ReadOnly] public int SectorCount;
        [ReadOnly] public int ActiveMeshCount;

        [Header("Model Preview Information")]
        [ReadOnly] public int ModelChunkIndex;
        [ReadOnly] public int ModelChunkCount;

        private ReconstructionRenderMode renderMode = ReconstructionRenderMode.NONE;
        private ReconstructionRenderMode last_renderMode = ReconstructionRenderMode.NONE;
		
        private int LastProcessedFrame = 0;

        private List<int> OrderedList = new List<int>();

        // Data
        private float[] VertexData;
        private int[] IndexData;
        private int[] SectorIDList;
        private int[] SectorVertNum;
        private int[] SectorMeshIdNum;
        private GameObject LiveMeshesGroups = null;   // put all mesh inside
        private float[] ModelVertices;
        private int[] ModelIndices;
        
        private static Dictionary<int, GameObject> ShowGameObjs = new Dictionary<int, GameObject>();
        private static Dictionary<int, MeshFilter> ShowMeshFilters = new Dictionary<int, MeshFilter>();
        private Material UsingMaterial;

        private readonly object MeshLock = new object();

        #region Multi-Thread Get Mesh Data
        // Multi-thread Parse Raw Data to Data List for Mesh object 
        private Thread MeshDataThread = null;
        private Coroutine MeshDataCoroutine = null; // IEnumerator for main thread Mesh
        private int NumSubMeshes = 0;
        private int LastMeshes = 0;
        private int NumLastMeshVert = 0;
        private int VertID = 0;
        private int MeshIdxID = 0;
        private static Dictionary<int, List<Vector3>> MeshDataVertices = new Dictionary<int, List<Vector3>>();
        private static Dictionary<int, List<int>> MeshDataIndices = new Dictionary<int, List<int>>();
        private static Dictionary<int, List<Color32>> MeshDataColors = new Dictionary<int, List<Color32>>();
        private static Dictionary<int, List<Vector3>> MeshDataNormals = new Dictionary<int, List<Vector3>>();

        private bool IsMeshUpdate = false;
        private bool IsCoroutineRunning = false;
        private bool IsThreadRunning = true;
        #endregion

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable FullSceneQuality has changed to FullScenePointQuality.
        * @warning The variable will remove in the future.
        */
        public ReconstructionQuality FullSceneQuality
        {
            get { return FullScenePointQuality; }
            private set { }
        }
        /**
        * The variable VertexNum has changed to VertexCount.
        * @warning The variable will remove in the future.
        */
        public int VertexNum
        {
            get { return VertexCount; }
            private set { }
        }
        /**
        * The variable IndexNum has changed to IndexCount.
        * @warning The variable will remove in the future.
        */
        public int IndexNum
        {
            get { return IndexCount; }
            private set { }
        }
        /**
        * The variable ProcessedFrame has changed to CurrentFrameID.
        * @warning The variable will remove in the future.
        */
        public int ProcessedFrame
        {
            get { return CurrentFrameID; }
            private set { }
        }
        /**
        * The variable ColliderNum has changed to ColliderCount.
        * @warning The variable will remove in the future.
        */
        public int ColliderNum
        {
            get { return ColliderCount; }
            private set { }
        }
        /**
        * The variable SectorNum has changed to SectorCount.
        * @warning The variable will remove in the future.
        */
        public int SectorNum
        {
            get { return SectorCount; }
            private set { }
        }
        /**
        * The variable NumOfActiveGO has changed to ActiveMeshCount.
        * @warning The variable will remove in the future.
        */
        public int NumOfActiveGO
        {
            get { return ActiveMeshCount; }
            private set { }
        }
        /**
        * The variable ModelChunkIdx has changed to ModelChunkIndex.
        * @warning The variable will remove in the future.
        */
        public int ModelChunkIdx
        {
            get { return ModelChunkIndex; }
            private set { }
        }
        /**
        * The variable ModelChunkNum has changed to ModelChunkCount.
        * @warning The variable will remove in the future.
        */
        public int ModelChunkNum
        {
            get { return ModelChunkCount; }
            private set { }
        }
        #endregion

        private ViveSR_RigidReconstructionRenderer() { }
        private static ViveSR_RigidReconstructionRenderer Mgr = null;
        public static ViveSR_RigidReconstructionRenderer Instance
        {
            get
            {
                if (Mgr == null)
                {
                    Mgr = FindObjectOfType<ViveSR_RigidReconstructionRenderer>();
                }
                if (Mgr == null)
                {
                    Debug.LogError("ViveSR_RigidReconstructionRenderer does not be attached on GameObject");
                }
                return Mgr;
            }
        }

        // set self-setting to the static param
        public override bool RightBeforeStartModule()
        {
            bool result = ViveSR_RigidReconstruction.InitRigidReconstructionParamFromFile(ConfigFilePath);
            if (!result)
            {
                Debug.Log("[ViveSR] [RigidReconstruction] Set Config By Config File");
            }
            else
            {
                Debug.Log("[ViveSR] [RigidReconstruction] Config File Not Found, Set Config From GameObject");
                SRWorkModule_API.SetReconstructionParameterInt((int)ReconstructionParam.CONFIG_QUALITY, (int)FullScenePointQuality);
            }

            WireframeMaterial.SetFloat("_Opaque", SetWireFrameOpaque ? 1.0f : 0.0f);
            return true;
        }

        private bool UpdateRuntimeParameter()
        {
            bool result = true;
            int ret = (int)Error.FAILED;
            // check live mesh display mode
            if (ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingView || ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                LiveMeshDisplayMode = ReconstructionDisplayMode.ADAPTIVE_MESH; //support only in this mode

            // update live mesh display mode
            if ((int)LiveMeshDisplayMode != LastLiveMeshDisplayMode)
            {
                HideAllLiveMeshes();
                result = SetMeshDisplayMode(LiveMeshDisplayMode) && result;
                LastLiveMeshDisplayMode = (int)LiveMeshDisplayMode;
                // refresh rate
                SRWorkModule_API.SetReconstructionParameterInt((int)ReconstructionParam.MESH_REFRESH_INTERVAL, RefreshIntervalMS);
            }
            // full scene quality
            if (LiveMeshDisplayMode == ReconstructionDisplayMode.FULL_SCENE)
            {
                ret = SRWorkModule_API.SetReconstructionParameterInt((int)(ReconstructionParam.CONFIG_QUALITY), (int)FullScenePointQuality);
                LiveMeshMaterial.SetFloat("_PointSizeScaler", (FullScenePointQuality == ReconstructionQuality.LOW) ? 1.2f : 0.8f);
                result = result && (ret == (int)Error.WORK);
            }
            // update live adaptive param
            if (LiveMeshDisplayMode == ReconstructionDisplayMode.ADAPTIVE_MESH)
            {
                ret = SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MAX_GRID, ViveSR_RigidReconstruction.LiveAdaptiveMaxGridSize * 0.01f);   // cm to m
                result = result && (ret == (int)Error.WORK);
                ret = SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_MIN_GRID, ViveSR_RigidReconstruction.LiveAdaptiveMinGridSize * 0.01f);
                result = result && (ret == (int)Error.WORK);
                ret = SRWorkModule_API.SetReconstructionParameterFloat((int)ReconstructionParam.ADAPTIVE_ERROR_THRES, ViveSR_RigidReconstruction.LiveAdaptiveErrorThres);
                result = result && (ret == (int)Error.WORK);
            }

            if (EnableSector != LastEnableSector)
            {
                HideAllLiveMeshes();
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.ENABLE_FRUSTUM_CULLING), EnableSector);
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.ENABLE_SECTOR_GROUPER), EnableSector);
                SRWorkModule_API.SetReconstructionParameterFloat((int)(ReconstructionParam.SECTOR_SIZE), SectorSizeInMeter);
                LastEnableSector = EnableSector;
            }

            if (SetWireFrameOpaque != LastSetWireFrameOpaque)
            {
                WireframeMaterial.SetFloat("_Opaque", SetWireFrameOpaque ? 1.0f : 0.0f);
                LastSetWireFrameOpaque = SetWireFrameOpaque;
            }

            if (SetWireFrameThickness != LastSetWireFrameThickness)
            {
                WireframeMaterial.SetFloat("_Thickness", SetWireFrameThickness);
                LastSetWireFrameThickness = SetWireFrameThickness;
            }

            return result;
        }

        private void ResetData()
        {
            VertexCount = 0;
            IndexCount = 0;
            SectorCount = 0;
            ActiveMeshCount = 0;
        }

        public bool SetMeshDisplayMode(ReconstructionDisplayMode displayMode)
        {
            ResetData();

            int result = (int)Error.FAILED;
            if (displayMode == ReconstructionDisplayMode.FIELD_OF_VIEW)
            {
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.ENABLE_FRUSTUM_CULLING), false);
                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.ENABLE_SECTOR_GROUPER), false);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LITE_POINT_CLOUD_MODE), true);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.FULL_POINT_CLOUD_MODE), false);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LIVE_ADAPTIVE_MODE), false);
                LiveMeshMaterial.SetFloat("_PointSizeScaler", 1.2f);
                UsingMaterial = LiveMeshMaterial;
                ThreadPeriod = 15;
                BackupSectorValue = EnableSector;
                EnableSector = false;
            }
            else if (displayMode == ReconstructionDisplayMode.FULL_SCENE)
            {
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LITE_POINT_CLOUD_MODE), false);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.FULL_POINT_CLOUD_MODE), true);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LIVE_ADAPTIVE_MODE), false);
                LiveMeshMaterial.SetFloat("_PointSizeScaler", (FullScenePointQuality == ReconstructionQuality.LOW) ? 1.3f : 0.8f);
                UsingMaterial = LiveMeshMaterial;
                ThreadPeriod = 300;
                EnableSector = BackupSectorValue;
            }
            else if (displayMode == ReconstructionDisplayMode.ADAPTIVE_MESH)
            {
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LITE_POINT_CLOUD_MODE), false);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.FULL_POINT_CLOUD_MODE), false);
                result = SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionParam.LIVE_ADAPTIVE_MODE), true);
                UsingMaterial = WireframeMaterial;
                ThreadPeriod = 300;
                EnableSector = BackupSectorValue;
            }
            foreach (KeyValuePair<int, GameObject> go in ShowGameObjs)
            {
                go.Value.GetComponent<MeshRenderer>().sharedMaterial = UsingMaterial;
            }

            if (result == (int)Error.WORK) { LiveMeshDisplayMode = displayMode; }

            return (result == (int)Error.WORK);
        }
        public void SetMeshMaterial(Material mat)
        {
            UsingMaterial = mat;
            foreach (KeyValuePair<int, GameObject> go in ShowGameObjs)
            {
                go.Value.GetComponent<MeshRenderer>().sharedMaterial = UsingMaterial;
            }
        }

        // Use this for initialization
        public override bool Initial()
        {
            if (!ViveSR.Instance.InitializeRigidReconstructionModule)
            {
                return false;
            }
            if (ViveSR.FrameworkStatus == FrameworkStatus.WORKING)
            {
                ViveSR_RigidReconstruction.AllocOutputDataMemory();

                // Init Shader Variant
                Shader.EnableKeyword("RENDER_AS_BILLBOARD");

                if (LiveMeshesGroups == null)
                {
                    LiveMeshesGroups = new GameObject("LiveMeshes");
                    LiveMeshesGroups.transform.SetParent(gameObject.transform, false);
                }

                if(ShowMeshFilters == null) 
                    ShowMeshFilters = new Dictionary<int, MeshFilter>();
                
                if(ShowGameObjs == null)
                    ShowGameObjs = new Dictionary<int, GameObject>();

                LiveMeshesGroups.SetActive(true);

                LiveMeshDisplayMode = ReconstructionDisplayMode.ADAPTIVE_MESH;
                SetMeshDisplayMode(LiveMeshDisplayMode);

                IsMeshUpdate = false;
                IsCoroutineRunning = false;

                BackupSectorValue = EnableSector;

                return true;
            }
            
            return false;
            
        }

        // Update is called once per frame
        void Update()
        {
            if (ViveSR_RigidReconstruction.IsExporting || ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo || !ViveSR_RigidReconstruction.IsScanning)
            {
                if(renderMode != ReconstructionRenderMode.MODEL_PREVIEW) HideAllLiveMeshes();
            }
            if (ViveSR_RigidReconstruction.IsScanning || ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
            {
                // when exporting, don't update live extraction parameter                
                UpdateRuntimeParameter();
                if (IsMeshUpdate == true)
                    MeshDataCoroutine = StartCoroutine(RenderMeshDataIEnumerator());
            }
        }
        public void ResetParameter()
        {
            EnableSector = true;
            LastEnableSector = false;
            BackupSectorValue = true;
            SetWireFrameOpaque = true;
            LastSetWireFrameOpaque = true;
            NumSubMeshes = 0;
            LastMeshes = 0;
            NumLastMeshVert = 0;
            VertID = 0;
            MeshIdxID = 0;
        }
        public override bool Release()
        {
            if (!ViveSR.Instance.InitializeRigidReconstructionModule)
            {
                return false;
            }
            ViveSR_RigidReconstruction.StopScanning();
            ViveSR_SceneUnderstanding.ResetParameter();
            IsThreadRunning = false;
            if (MeshDataThread != null)
            {
                MeshDataThread.Join();
                MeshDataThread.Abort();
                MeshDataThread = null;
            }
            if (IsCoroutineRunning == true)
            {
                StopCoroutine(MeshDataCoroutine);
                MeshDataCoroutine = null;
            }
            ViveSR_RigidReconstruction.ReleaseAllocOutputDataMemory();

            foreach (KeyValuePair<int, GameObject> data in ShowGameObjs)
            {
                int id = data.Key;

                MeshDataVertices[id].Clear();
                MeshDataIndices[id].Clear();
                MeshDataColors[id].Clear();
                MeshDataNormals[id].Clear();
            }
            OrderedList.Clear();
            MeshDataVertices.Clear();
            MeshDataIndices.Clear();
            MeshDataColors.Clear();
            MeshDataNormals.Clear();
            HideAllLiveMeshes();
            DestroyLiveMeshes();
            ResetParameter();
            return true;
        }

        private static void HideAllLiveMeshes()
        {
            foreach (KeyValuePair<int, GameObject> data in ShowGameObjs)
            {
                if (data.Value != null)
                {
                    data.Value.SetActive(false);
                    if (data.Value.GetComponent<MeshCollider>() != null)
                        data.Value.GetComponent<MeshCollider>().enabled = false;
                }
            }
        }

        private static void DestroyLiveMeshes()
        {

            foreach (KeyValuePair<int, GameObject> data in ShowGameObjs)
            {
                if (data.Value != null)
                {
                    GameObject.Destroy(data.Value);
                }
            }
            foreach (KeyValuePair<int, MeshFilter> data in ShowMeshFilters)
            {
                if (data.Value != null)
                {
                    GameObject.Destroy(data.Value);
                }
            }
            ShowMeshFilters.Clear();
            ShowGameObjs.Clear();
            ShowMeshFilters = null;
            ShowGameObjs = null; 
        }

        private IEnumerator RenderMeshDataIEnumerator()
        {
            IsCoroutineRunning = true;

            if (IsMeshUpdate == true)
            {
                // it is basically try lock, use try lock to avoid spending too much time in update()
                if (Monitor.TryEnter(MeshLock)) {
                    try {
                        ActiveMeshCount = 0;
                        if (last_renderMode != renderMode)
                        {
                            HideAllLiveMeshes();
                            last_renderMode = renderMode;
                        }

                        if (renderMode == ReconstructionRenderMode.SECTOR)
                        {
                            for (int i = 0; i < SectorCount; i++)
                                SetMeshData(SectorIDList[i]);
                            // Take out key one by one. Either add new mesh or add age of meshes that are not updated.
                            UpdateMeshAge();
                            // Enable/Disable collider if user set boolean in runtime
                            if(PrevLiveMeshCollider != LiveMeshCollider)
                            {
                                foreach (KeyValuePair<int, GameObject> go in ShowGameObjs)
                                {
                                    if(go.Value.GetComponent<MeshCollider>())
                                    {
                                        go.Value.GetComponent<MeshCollider>().enabled = LiveMeshCollider;
                                    }
                                }
                            }
                            PrevLiveMeshCollider = LiveMeshCollider;
                        }
                        else if (renderMode == ReconstructionRenderMode.MODEL_PREVIEW)
                        {
                            SetMeshData(ModelChunkIndex);
                        }
                        else
                        {
                            for (int i = 0; i < NumSubMeshes; i++)
                                SetMeshData(i);
                        }

                        foreach (KeyValuePair<int, GameObject> go in ShowGameObjs)
                        {
                            go.Value.GetComponent<MeshRenderer>().sharedMaterial = UsingMaterial;
                        }

                        IsMeshUpdate = false;
                    }
                    finally {
                        Monitor.Exit(MeshLock);
                    }
                }
            }
            IsCoroutineRunning = false;
            yield return 0;
        }

        private void SetMeshData(int id)
        {
            if (!ShowGameObjs.ContainsKey(id))
            {
                AddNewMesh(id);
            }
            ShowGameObjs[id].SetActive(true);

            ShowMeshFilters[id].sharedMesh.Clear();
            ShowMeshFilters[id].sharedMesh.SetVertices(MeshDataVertices[id]);
            ShowMeshFilters[id].sharedMesh.SetColors(MeshDataColors[id]);
            bool topologyIsTrangle = (LiveMeshDisplayMode == ReconstructionDisplayMode.ADAPTIVE_MESH && (IndexCount > 0) || renderMode == ReconstructionRenderMode.MODEL_PREVIEW);
            ShowMeshFilters[id].sharedMesh.SetIndices(MeshDataIndices[id].ToArray(), topologyIsTrangle? MeshTopology.Triangles : MeshTopology.Points, 0);
            if (MeshDataNormals[id].Count > 0)
            {
                ShowMeshFilters[id].sharedMesh.SetNormals(MeshDataNormals[id]);
            }

            if (renderMode == ReconstructionRenderMode.MODEL_PREVIEW)
            {
                if (ShowGameObjs[id].GetComponent<MeshCollider>())
                {
                    ShowGameObjs[id].GetComponent<MeshCollider>().enabled = true;
                }
                else
                {
                    ShowGameObjs[id].AddComponent<MeshCollider>();
                }
            }
            if (renderMode == ReconstructionRenderMode.SECTOR && LiveMeshCollider)
            {
                if (ShowGameObjs[id].GetComponent<MeshCollider>())
                {
                    ShowGameObjs[id].GetComponent<MeshCollider>().enabled = true;
                    Mesh MyMesh = ShowGameObjs[id].GetComponent<MeshFilter>().sharedMesh;
                    ShowGameObjs[id].GetComponent<MeshCollider>().sharedMesh = null;
                    ShowGameObjs[id].GetComponent<MeshCollider>().sharedMesh = MyMesh;
                }
                else
                {
                    ShowGameObjs[id].AddComponent<MeshCollider>();
                }
            }
        }

        private void AddNewMesh(int id)
        {
            GameObject go = new GameObject("SubMesh_" + id);
            go.transform.SetParent(LiveMeshesGroups.transform, false);

            MeshRenderer renderer = go.AddComponent<MeshRenderer>();
            renderer.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
            renderer.sharedMaterial = LiveMeshMaterial;

            MeshFilter filter = go.AddComponent<MeshFilter>();
            filter.mesh = new Mesh();
            filter.mesh.MarkDynamic();

            ShowGameObjs.Add(id, go);
            ShowMeshFilters.Add(id, filter);
        }

        private void UpdateMeshAge()
        {
            for (int i = OrderedList.Count - 1; i >= 0 ; i--)
            {
                for (int j = SectorCount - 1; j >= 0 ; j--)
                {
                    if (OrderedList[i] == SectorIDList[j])
                    {
                        OrderedList.RemoveAt(i);
                        break;
                    }
                }
            }

            // Insert updated game obj in with age 0
            for (int i = 0; i < SectorCount; i++)
            {
                OrderedList.Insert(0, SectorIDList[i]);
            }

            // Hide the oldest ones that are over max number of game objects
            for (int i = MaxActiveGO; i < OrderedList.Count; i++)
            {
                int id = OrderedList[i];
                ShowGameObjs[id].SetActive(false);
            }

            ActiveMeshCount = Math.Min(OrderedList.Count, MaxActiveGO);
        }

        public void ExtractMeshData()
        {
            try
            {
                if (IsMeshUpdate == false && ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                {
                    bool result = ViveSR_RigidReconstruction.GetScannedModelPreviewData(
                        ref ModelChunkCount, ref ModelChunkIndex, ref VertexCount, out VertexData, ref IndexCount, out IndexData);

                    if (result)
                    {
                        lock (MeshLock) {
                            if (VertexCount > 0)
                                UpdateMeshesDataList();

                            if (ModelPreviewIsCompleted())
                            {
                                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.MODEL_PREVIEW_FINISH), true); // to stop getting data from SRWorks
                                ModelChunkCount = 0;
                            }
                            else
                            {
                                SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.MODEL_PREVIEW_NEXT_CHUNK), true);
                            }
                        }
                    }
                }
                else if (IsMeshUpdate == false && ViveSR_RigidReconstruction.IsScanning == true)
                {
                    bool result = ViveSR_RigidReconstruction.GetRigidReconstructionFrame(ref CurrentFrameID);
                    if (CurrentFrameID != LastProcessedFrame && result == true)
                    {
                        LastProcessedFrame = CurrentFrameID;
                        float[] _camPose;

                        result = ViveSR_RigidReconstruction.GetRigidReconstructionData(
                            ref CurrentFrameID, out _camPose, ref VertexCount, out VertexData, ref VertStrideInFloat,
                            out SectorIDList, ref SectorCount, out SectorVertNum, out SectorMeshIdNum, ref IndexCount, out IndexData);

                        if (result == true)
                        {
                            lock (MeshLock) {
                                NumSubMeshes = 0;
                                LastProcessedFrame = CurrentFrameID;

                                if (LiveMeshDisplayMode != ReconstructionDisplayMode.ADAPTIVE_MESH)
                                    UpdatePointCloudDataList();
                                else if (IndexCount > 0)
                                    UpdateMeshesDataList();
                            }
                        }
                    }
                }
            }
            catch (System.Exception e)
            {
                NumSubMeshes = 0;
                Debug.LogWarning(e.Message);
            }
        }

        private void ExtractMeshDataThread()
        {
            while (IsThreadRunning == true)
            {
                try
                {
                    if (IsMeshUpdate == false && ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                    {
                        if (RigidReconstruction.SRWork_Rigid_Reconstruciton.UpdateData()) {
                            bool result = ViveSR_RigidReconstruction.GetScannedModelPreviewData(
                                ref ModelChunkCount, ref ModelChunkIndex, ref VertexCount, out VertexData, ref IndexCount, out IndexData);

                            if (result)
                            {
                                lock (MeshLock) {
                                    if (VertexCount > 0)
                                        UpdateMeshesDataList();

                                    if (ModelPreviewIsCompleted())
                                    {
                                        SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.MODEL_PREVIEW_FINISH), true); // to stop getting data from SRWorks
                                        ModelChunkCount = 0;
                                    }
                                    else
                                    {
                                        SRWorkModule_API.SetReconstructionParameterBool((int)(ReconstructionCmd.MODEL_PREVIEW_NEXT_CHUNK), true);
                                    }
                                }
                            }
                        }
                    }
                    else if (IsMeshUpdate == false && ViveSR_RigidReconstruction.IsScanning == true)
                    {
                        if (RigidReconstruction.SRWork_Rigid_Reconstruciton.UpdateData()) {
                            bool result = ViveSR_RigidReconstruction.GetRigidReconstructionFrame(ref CurrentFrameID);
                            if (CurrentFrameID != LastProcessedFrame && result == true)
                            {
                                LastProcessedFrame = CurrentFrameID;
                                float[] _camPose;

                                result = ViveSR_RigidReconstruction.GetRigidReconstructionData(
                                    ref CurrentFrameID, out _camPose, ref VertexCount, out VertexData, ref VertStrideInFloat,
                                    out SectorIDList, ref SectorCount, out SectorVertNum, out SectorMeshIdNum, ref IndexCount, out IndexData);

                                if (result == true)
                                {
                                    lock (MeshLock) {
                                        NumSubMeshes = 0;
                                        LastProcessedFrame = CurrentFrameID;

                                        if (LiveMeshDisplayMode != ReconstructionDisplayMode.ADAPTIVE_MESH)
                                            UpdatePointCloudDataList();
                                        else if (IndexCount > 0)
                                            UpdateMeshesDataList();
                                    }
                                }
                            }
                        }
                    }
                }
                catch (System.Exception e)
                {
                    NumSubMeshes = 0;
                    Debug.LogWarning(e.Message);
                }

                Thread.Sleep(ThreadPeriod); //Avoid too fast get data from SR SDK DLL 
            }
        }

        private bool ModelPreviewIsCompleted()
        {
            return (ModelChunkIndex == ModelChunkCount - 1) || (ModelChunkCount == 0); // normal or abnormal finish in SRWorks
        }

        private void UpdateRenderMode()
        {
            if (ModelChunkCount > 0)
            {
                renderMode = ReconstructionRenderMode.MODEL_PREVIEW; /*Debug.Log("model preview mode");*/
            }
            else if (EnableSector && SectorCount > 0)
            {
                renderMode = ReconstructionRenderMode.SECTOR; /*Debug.Log("sector mode");*/
            }
            else if (!EnableSector && SectorCount == 0)
            {
                renderMode = ReconstructionRenderMode.ALL; /*Debug.Log("default mode"); */
            }
            else
            {
                renderMode = ReconstructionRenderMode.NONE;  // do not render incomplete data
            }
        }

        private void UpdateMeshesDataList()
        {
            UpdateRenderMode();

            if (renderMode == ReconstructionRenderMode.MODEL_PREVIEW)
            {
                UpdateMeshesDataList_ModelPreview();
                IsMeshUpdate = true;
            }
            else if (renderMode == ReconstructionRenderMode.SECTOR)
            {
                UpdateMeshesDataList_Sector();
                IsMeshUpdate = true;
            }
            else if (renderMode == ReconstructionRenderMode.ALL)
            {
                UpdateMeshesDataList_AllData();
                IsMeshUpdate = true;
            }
        }

        private void UpdateMeshesDataList_ModelPreview()
        {
            int VertStride = 4;
            Vector3 vertexDst = new Vector3();
            Color32 colorDst = new Color32();
            NumSubMeshes = ModelChunkIndex;
            if (!MeshDataVertices.ContainsKey(NumSubMeshes))
            {
                MeshDataVertices[NumSubMeshes] = new List<Vector3>();
                MeshDataIndices[NumSubMeshes] = new List<int>();
                MeshDataColors[NumSubMeshes] = new List<Color32>();
                MeshDataNormals[NumSubMeshes] = new List<Vector3>();
            }
            else
            {
                MeshDataVertices[NumSubMeshes].Clear();
                MeshDataIndices[NumSubMeshes].Clear();
                MeshDataColors[NumSubMeshes].Clear();
                MeshDataNormals[NumSubMeshes].Clear();
            }
            for (int i = 0; i < VertexCount; i++)
            {
                int startOffset = i * VertStride;
                float x = VertexData[startOffset + 0];
                float y = VertexData[startOffset + 1];
                float z = VertexData[startOffset + 2];
                vertexDst.Set(x, y, z);
                MeshDataVertices[NumSubMeshes].Add(vertexDst);

                byte[] bits = BitConverter.GetBytes(VertexData[startOffset + 3]);
                colorDst.r = bits[0];
                colorDst.g = bits[1];
                colorDst.b = bits[2];
                colorDst.a = bits[3];
                MeshDataColors[NumSubMeshes].Add(colorDst);

            }
            for (int j = 0; j < IndexCount; j++)
            {
                int id = IndexData[j];
                MeshDataIndices[NumSubMeshes].Add(id);
            }
        }

        private void UpdateMeshesDataList_Sector()
        {
            Color32 colorDst = new Color32();

            VertID = 0;
            MeshIdxID = 0;
            for (int i = 0; i < SectorCount; i++)
            {
                int meshID = SectorIDList[i];
                if (!MeshDataVertices.ContainsKey(meshID))
                {
                    MeshDataVertices[meshID] = new List<Vector3>();
                    MeshDataIndices[meshID] = new List<int>();
                    MeshDataColors[meshID] = new List<Color32>();
                    MeshDataNormals[meshID] = new List<Vector3>();
                }
                else
                {
                    MeshDataVertices[meshID].Clear();
                    MeshDataIndices[meshID].Clear();
                    MeshDataColors[meshID].Clear();
                    MeshDataNormals[meshID].Clear();
                }

                int vert_num = SectorVertNum[i];
                int idx_num = SectorMeshIdNum[i];

                for (int j = 0; j < vert_num; j++)
                {
                    int startOffset = VertID * VertStrideInFloat;
                    float x = VertexData[startOffset + 0];
                    float y = VertexData[startOffset + 1];
                    float z = VertexData[startOffset + 2];
                    MeshDataVertices[meshID].Add(new Vector3(x, y, z));
                    VertID++;

                    byte[] bits = BitConverter.GetBytes(VertexData[startOffset + 3]);
                    colorDst.r = bits[0];
                    colorDst.g = bits[1];
                    colorDst.b = bits[2];
                    colorDst.a = bits[3];
                    MeshDataColors[meshID].Add(colorDst);
                }

                for (int j = 0; j < idx_num; j++)
                {
                    MeshDataIndices[meshID].Add(IndexData[MeshIdxID]);
                    MeshIdxID++;
                }
            }
        }

        private void UpdateMeshesDataList_AllData()
        {
            Color32 colorDst = new Color32();

            List<int> idMapping = Enumerable.Repeat(-1, VertexCount).ToList();

            int triNum = IndexCount / 3;
            int numSubVert = 0;
            int numSubTri = 0;

            if (!MeshDataVertices.ContainsKey(NumSubMeshes))
            {
                MeshDataVertices[NumSubMeshes] = new List<Vector3>();
                MeshDataIndices[NumSubMeshes] = new List<int>();
                MeshDataColors[NumSubMeshes] = new List<Color32>();
                MeshDataNormals[NumSubMeshes] = new List<Vector3>();
            }
            else
            {
                MeshDataVertices[NumSubMeshes].Clear();
                MeshDataIndices[NumSubMeshes].Clear();
                MeshDataColors[NumSubMeshes].Clear();
                MeshDataNormals[NumSubMeshes].Clear();
            }

            for (int triID = 0; triID < triNum; ++triID)
            {
                // if this iteration will exceed the limitation, output to a new geometry first
                if ((numSubVert + 3) > 65000 || (numSubTri + 1) > 65000)
                {
                    NumSubMeshes++;

                    if (!MeshDataVertices.ContainsKey(NumSubMeshes))
                    {

                        MeshDataVertices[NumSubMeshes] = new List<Vector3>();
                        MeshDataIndices[NumSubMeshes] = new List<int>();
                        MeshDataColors[NumSubMeshes] = new List<Color32>();
                        MeshDataNormals[NumSubMeshes] = new List<Vector3>();
                    }
                    else
                    {
                        MeshDataVertices[NumSubMeshes].Clear();
                        MeshDataIndices[NumSubMeshes].Clear();
                        MeshDataColors[NumSubMeshes].Clear();
                        MeshDataNormals[NumSubMeshes].Clear();
                    }

                    // clear the counter etc
                    idMapping = Enumerable.Repeat(-1, VertexCount).ToList();
                    numSubVert = numSubTri = 0;
                }

                for (uint i = 0; i < 3; ++i)
                {
                    // insert vertices and get new ID
                    int vertID = IndexData[triID * 3 + i];
                    //if (vertID >= VertexNum) 
                    //{
                    //    Debug.LogWarning("vertID:" + vertID + ", vert Num:" + VertexNum);    // a known bug, caught by exception
                    //    NumSubMeshes = 0; return;   
                    //}
                    if (idMapping[vertID] == -1)                // haven't added this vertex yet
                    {
                        idMapping[vertID] = MeshDataVertices[NumSubMeshes].Count;   // old ID -> new ID
                        float x = VertexData[vertID * VertStrideInFloat + 0];
                        float y = VertexData[vertID * VertStrideInFloat + 1];
                        float z = VertexData[vertID * VertStrideInFloat + 2];
                        MeshDataVertices[NumSubMeshes].Add(new Vector3(x, y, z));

                        byte[] bits = BitConverter.GetBytes(VertexData[vertID * VertStrideInFloat + 3]);
                        colorDst.r = bits[0];
                        colorDst.g = bits[1];
                        colorDst.b = bits[2];
                        colorDst.a = bits[3];
                        MeshDataColors[NumSubMeshes].Add(colorDst);

                        ++numSubVert;
                    }
                    MeshDataIndices[NumSubMeshes].Add(idMapping[vertID]);
                }
                ++numSubTri;
            }

            ++NumSubMeshes;
        }

        private void UpdatePointCloudDataList()
        {
            UpdateRenderMode();

            // vertStrideInFloat > 4 ==> has normal component
            bool withNormal = (VertStrideInFloat > 4);
            VertID = 0;

            if (renderMode == ReconstructionRenderMode.SECTOR)
            {
                for (int i = 0; i < SectorCount; i++)
                {
                    UpdateSinglePointCloudData(SectorIDList[i], SectorVertNum[i], withNormal);
                }
                IsMeshUpdate = true;
            }
            else if (renderMode == ReconstructionRenderMode.ALL)
            {
                NumSubMeshes = (int)Math.Ceiling((float)VertexCount / 65000);
                LastMeshes = (NumSubMeshes * 65000 == VertexCount) ? NumSubMeshes + 1 : NumSubMeshes;
                NumLastMeshVert = VertexCount - (65000 * (LastMeshes - 1));
                for (int i = 0; i < NumSubMeshes; i++)
                {
                    int numVerts = (i == NumSubMeshes - 1) ? NumLastMeshVert : 65000;
                    UpdateSinglePointCloudData(i, numVerts, withNormal);
                }
                IsMeshUpdate = true;
            }
        }

        private void UpdateSinglePointCloudData(int meshID, int numVert, bool withNormal)
        {
            Vector3 vertexDst = new Vector3();
            Color32 colorDst = new Color32();
            Vector3 normalDst = new Vector3();

            if (!MeshDataVertices.ContainsKey(meshID))
            {
                MeshDataVertices[meshID] = new List<Vector3>();
                MeshDataIndices[meshID] = new List<int>();
                MeshDataColors[meshID] = new List<Color32>();
                MeshDataNormals[meshID] = new List<Vector3>();
            }
            else
            {
                MeshDataVertices[meshID].Clear();
                MeshDataIndices[meshID].Clear();
                MeshDataColors[meshID].Clear();
                MeshDataNormals[meshID].Clear();
            }            

            for (int i = 0; i < numVert; ++i)
            {
                //int vertID = meshID * 65000 + i;
                int startOffset = VertID * VertStrideInFloat;
                float x = VertexData[startOffset + 0];
                float y = VertexData[startOffset + 1];
                float z = VertexData[startOffset + 2];
                vertexDst.Set(x, y, z);
                MeshDataVertices[meshID].Add(vertexDst);

                byte[] bits = BitConverter.GetBytes(VertexData[startOffset + 3]);
                colorDst.r = bits[0];
                colorDst.g = bits[1];
                colorDst.b = bits[2];
                colorDst.a = bits[3];
                MeshDataColors[meshID].Add(colorDst);

                if (withNormal)
                {
                    float norX = VertexData[startOffset + 4];
                    float norY = VertexData[startOffset + 5];
                    float norZ = VertexData[startOffset + 6];
                    normalDst.Set(norX, norY, norZ);
                    MeshDataNormals[meshID].Add(normalDst);
                }

                MeshDataIndices[meshID].Add(i);
                VertID++;
            }
        }
    }
}
