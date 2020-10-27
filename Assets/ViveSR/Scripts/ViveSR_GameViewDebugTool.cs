//========= Copyright 2017, HTC Corporation. All rights reserved. ===========
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_GameViewDebugTool : MonoBehaviour
    {
        private bool EnableDebugTool = false;
        private int ToolbarIndex = 0;
        private string[] ToolbarLabels = new string[] { "PassThrough", "Depth", "3D", "SceneUnderstanding" };
        private const short AreaXStart = 10;
        private const short AreaYStart = 45;

        bool set_depth_fps = false;
        float depth_fps = 30.0f;

        private void Update()
        {
            if (Input.GetKey(KeyCode.S) && Input.GetKeyDown(KeyCode.R)) EnableDebugTool = !EnableDebugTool;
        }

        private void OnGUI()
        {
            if (!EnableDebugTool) return;

            ToolbarIndex = GUI.Toolbar(new Rect(10, 10, 550, 30), ToolbarIndex, ToolbarLabels);
            GUILayout.Space(45);
            GUILayout.BeginArea(new Rect(AreaXStart, AreaYStart, Screen.width - AreaXStart, Screen.height - AreaYStart));
            switch (ToolbarIndex)
            {
                case 0:
                    PassThroughGUI();
                    break;
                case 1:
                    DepthGUI();
                    break;
                case 2:
                    ReconstructionGUI();
                    break;
                case 3:
                    SceneUnderstandingGUI();
                    break;
            }
            GUILayout.EndArea();
        }
        #region PassThroughGUI
        private void PassThroughGUI()
        {
            ViveSR_DualCameraImageRenderer.UpdateDistortedMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateDistortedMaterial, "Update Camera Material");
            ViveSR_DualCameraImageRenderer.UpdateUndistortedMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateUndistortedMaterial, "Update Undistorted Material");
            ViveSR_DualCameraImageRenderer.UpdateDepthMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateDepthMaterial, "Update Depth Material");
        }
        #endregion

        #region DepthGUI
        private void DepthGUI()
        {
            bool EnableDepth = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsDepthProcessing, "Depth Processing");
            if (ViveSR_DualCameraImageCapture.IsDepthProcessing != EnableDepth)
            {
                ViveSR_DualCameraImageCapture.EnableDepthProcess(EnableDepth);
            }

            if (ViveSR_DualCameraImageCapture.IsDepthProcessing)
            {
                ViveSR_DualCameraImageCapture.IsDepthRefinementEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsDepthRefinementEnabled, "Depth Refinement");
                ViveSR_DualCameraImageCapture.IsDepthEdgeEnhanceEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsDepthEdgeEnhanceEnabled, "Depth Edge Enhance");
            }

            ViveSR_DualCameraDepthCollider.UpdateDepthCollider = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.UpdateDepthCollider, "Depth Collider");
            if (ViveSR_DualCameraDepthCollider.UpdateDepthCollider)
            {
                ViveSR_DualCameraDepthCollider.UpdateDepthColliderRange = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.UpdateDepthColliderRange, "Depth Collider Range");
                ViveSR_DualCameraDepthCollider.DepthColliderVisibility = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.DepthColliderVisibility, "Show Depth Collider");

                GUILayout.Label("Set Mesh Near Distance:");
                GUILayout.BeginHorizontal();
                float NearDiatanceThres = ViveSR_DualCameraDepthCollider.UpdateColliderNearDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraDepthCollider.UpdateColliderNearDistance, 0.0f, 10.0f);
                GUILayout.Label("" + NearDiatanceThres.ToString("0.00"));
                GUILayout.EndHorizontal();

                GUILayout.Label("Set Mesh Far Distance:");
                GUILayout.BeginHorizontal();
                float FarDiatanceThres = ViveSR_DualCameraDepthCollider.UpdateColliderFarDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraDepthCollider.UpdateColliderFarDistance, 0.0f, 10.0f);
                GUILayout.Label("" + FarDiatanceThres.ToString("0.00"));
                GUILayout.EndHorizontal();
            }
            set_depth_fps = GUILayout.Toggle(set_depth_fps, "Set Depth FPS");
            if (set_depth_fps)
            {
                GUILayout.Box("Value: " + (int)Mathf.Round(depth_fps));
                float NewDepthFPS = GUILayout.HorizontalSlider(depth_fps, 1.0f, 60.0f);
                if (NewDepthFPS != depth_fps)
                {
                    SRWorkModule_API.SetDepthMaxFps((int)Mathf.Round(NewDepthFPS));
                    depth_fps = NewDepthFPS;
                }
            }
            if (ViveSR_DualCameraImageCapture.IsDepthProcessing)
            {
                ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled, "Start Recording Dataset");
                if (ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled)
                {
                    GUILayout.Label(new GUIContent("Recording Dataset..."));
                }
            }
            if (!ViveSR_DualCameraImageCapture.IsDepthProcessing && ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled)
                ViveSR_DualCameraImageCapture.EnableRecordingDataset(false);
        }
        #endregion

        #region ReconstructionGUI
        List<float> AdaptiveLevel = new List<float> { 64.0f, 32.0f, 16.0f, 8.0f, 4.0f, 2.0f };

        int MaxSelectID, MinSelectID;
        float ErrorThres, ExportMaxSize, ExportMinSize;

        string[] qualityList = new[] { "LOW - 4cm", "MID - 2cm", "HIGH - 1cm" };
        public Vive.Plugin.SR.ReconstructionQuality NewReconstructionUpdateQuality = Vive.Plugin.SR.ReconstructionQuality.MID;
        int selectQuality = 1;

        bool set_voxel_size = false;
        float geometrySize = 0.02f;
        float colorSize = 0.02f;

        private void ReconstructionGUI()
        {
            GUIStyle StyleBold = new GUIStyle { fontStyle = FontStyle.Bold };

            GUILayout.Label(new GUIContent("[Runtime Command]"), StyleBold);        // start / stop
            string BtnStrEnableReconstructionProcess = ViveSR_RigidReconstruction.ReconstructionProcessing ? "Disable Reconstruction Processing" : "Enable Reconstruction Processing";
            if (GUILayout.Button(BtnStrEnableReconstructionProcess, GUILayout.ExpandWidth(false)))
            {
                ViveSR_RigidReconstruction.EnableReconstructionProcess(!ViveSR_RigidReconstruction.ReconstructionProcessing);
            }
            if (ViveSR_RigidReconstruction.ReconstructionProcessing)
            {
                // Quality setting
                if (!ViveSR_RigidReconstruction.IsScanning)
                {
                    set_voxel_size = GUILayout.Toggle(set_voxel_size, "Set Voxel Size");
                    if (set_voxel_size)
                    {
                        selectQuality = GUILayout.SelectionGrid((int)NewReconstructionUpdateQuality, qualityList, 3);
                        ViveSR_RigidReconstruction.SetReconstructionUpdateQuality(selectQuality);

                        geometrySize = ViveSR_RigidReconstruction.GetReconstructionGeometrySize();
                        GUILayout.Label("Geometry resolution: " + geometrySize * 100 + " cm");
                        float NewGeometrySize = GUILayout.HorizontalSlider(geometrySize, 0.005f, 0.16f);
                        ViveSR_RigidReconstruction.SetReconstructionGeometrySize(NewGeometrySize);

                        colorSize = ViveSR_RigidReconstruction.GetReconstructionColorSize();
                        GUILayout.Label("Color resolution: " + colorSize * 100 + " cm");
                        float NewColorSize = GUILayout.HorizontalSlider(colorSize, 0.005f, 0.16f);
                        ViveSR_RigidReconstruction.SetReconstructionColorSize(NewColorSize);
                    }
                }

                GUILayout.Label(new GUIContent("--Start/Stop--"), StyleBold);
                if (!ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                {
                    if (GUILayout.Button("Start Reconstruction", GUILayout.ExpandWidth(false)))
                    {
                        ViveSR_RigidReconstruction.StartScanning();
                    }
                }
                if (ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                {
                    if (GUILayout.Button("Stop Reconstruction", GUILayout.ExpandWidth(false)))
                    {
                        ViveSR_RigidReconstruction.StopScanning();
                    }

                    GUILayout.Label(new GUIContent("--Live Extraction--"), StyleBold);
                    int curMode = (int)ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode;

                    if (curMode != (int)ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode)
                    {
                        ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode = (ReconstructionDisplayMode)curMode;
                    }
                    // adaptive tunning
                    if (curMode == (int)ReconstructionDisplayMode.ADAPTIVE_MESH)
                    {
                        GUILayout.Label(new GUIContent("--Live Adaptive Mesh Tuning--"), StyleBold);
                        DrawAdaptiveParamUI(ViveSR_RigidReconstruction.LiveAdaptiveMaxGridSize, ViveSR_RigidReconstruction.LiveAdaptiveMinGridSize, ViveSR_RigidReconstruction.LiveAdaptiveErrorThres);
                        ViveSR_RigidReconstruction.LiveAdaptiveMaxGridSize = AdaptiveLevel[MaxSelectID];
                        ViveSR_RigidReconstruction.LiveAdaptiveMinGridSize = AdaptiveLevel[MinSelectID];
                        ViveSR_RigidReconstruction.LiveAdaptiveErrorThres = ErrorThres;
                    }
                }

                // export
                if (ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
                {
                    GUILayout.Label(new GUIContent("--Export--"), StyleBold);
                    bool exportAdaptive = ViveSR_RigidReconstruction.ExportAdaptiveMesh;
                    ViveSR_RigidReconstruction.ExportAdaptiveMesh = GUILayout.Toggle(exportAdaptive, "Export Adaptive Model");

                    if (ViveSR_RigidReconstruction.ExportAdaptiveMesh)
                    {
                        // live extraction mode
                        GUILayout.Label(new GUIContent("--Export Adaptive Mesh Tuning--"), StyleBold);
                        DrawAdaptiveParamUI(ViveSR_RigidReconstruction.ExportAdaptiveMaxGridSize, ViveSR_RigidReconstruction.ExportAdaptiveMinGridSize, ViveSR_RigidReconstruction.ExportAdaptiveErrorThres);
                        ViveSR_RigidReconstruction.ExportAdaptiveMaxGridSize = AdaptiveLevel[MaxSelectID];
                        ViveSR_RigidReconstruction.ExportAdaptiveMinGridSize = AdaptiveLevel[MinSelectID];
                        ViveSR_RigidReconstruction.ExportAdaptiveErrorThres = ErrorThres;
                    }

                    if (GUILayout.Button("Start Export Model", GUILayout.ExpandWidth(false)))
                    {
                        ViveSR_RigidReconstruction.StopScanning();
                        ViveSR_RigidReconstruction.StartExporting("Model");
                    }
                }
            }
            if (ViveSR_RigidReconstruction.IsExporting || ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo)
            {
                if (ViveSR_RigidReconstruction.IsExporting) GUILayout.Label(new GUIContent("Exporting reconstruction model..."), StyleBold);
                if (ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo) GUILayout.Label(new GUIContent("Exporting scene understanding model..."), StyleBold);
                if (GUILayout.Button("Stop Export All Model"))
                {
                    ViveSR_RigidReconstruction.TerminateExporting();
                    ViveSR_RigidReconstruction.ResetReconstructionModule();
                }
            }
        }

        private void DrawAdaptiveParamUI(float MaxGridSize, float MinGridSize, float thres)
        {
            GUILayout.Label("Adaptive Range (Max~Min):");
            GUILayout.BeginHorizontal();
            MaxSelectID = AdaptiveLevel.IndexOf(MaxGridSize);
            MinSelectID = AdaptiveLevel.IndexOf(MinGridSize);
            GUILayout.EndHorizontal();

            GUILayout.Label("Divide Threshold:");
            GUILayout.BeginHorizontal();
            ErrorThres = GUILayout.HorizontalSlider(thres, 0.0f, 1.5f);
            GUILayout.Label("" + ErrorThres.ToString("0.00"));
            GUILayout.EndHorizontal();
        }
        #endregion


        #region SceneUnderstandingGUI        
        bool[] SceneObjectToggle = new[] { false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false };
        string[] SceneObjectName = new[] { "Floor", "Wall", "Ceiling", "Chair", "Table", "Bed", "Monitor", "Window", "Furniture", "Door", "Picture", "Person", "Light", "Plant", "Curtain", "Pillow" };
        string[] ObjectColor = new[] { "Green", "Blue", "Yellow", "Red", "Cyan", "Magenta", "Dark Red", "Dark Green", "Purple", "Pink", "Orange", "Grass Green", "Light yellow", "Mint Green", "Dark brown", "Dark blue" };

        private string ReconsSceneDir = "Recons3DAsset/";
        private string SemanticObjDir = "SemanticIndoorObj/";
        private string SemanticObjDirPath;
        private string ReconstructionResultDir = System.IO.Path.GetDirectoryName(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData)) + "\\LocalLow\\HTC Corporation\\SR_Reconstruction_Output\\";
        private void SceneUnderstandingGUI()
        {
            GUIStyle style_bold = new GUIStyle { fontStyle = FontStyle.Bold };
            GUILayout.Label(new GUIContent("--Scene Understanding--"), style_bold);
            if (ViveSR_RigidReconstruction.IsScanning)
            {
                bool IsSemanticEnabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding, "Enable Scene Understanding");
                if (IsSemanticEnabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstanding(IsSemanticEnabled, false);
                }
                bool IsSemanticRefinementEnabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingRefinement, "Enable Scene Understanding Refinement");
                if (IsSemanticRefinementEnabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingRefinement)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstandingRefinement(IsSemanticRefinementEnabled);
                }
            }
            else
            {
                GUILayout.Label(new GUIContent("Please Start Reconstruction"), style_bold);
            }
            if (ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding && ViveSR_RigidReconstruction.IsScanning)
            {
                GUILayout.Label(new GUIContent("--Scene Understanding Live Dispaly & Export--"), style_bold);
                bool is_semantic_preview_enabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingView, "Enable Preview");
                if (is_semantic_preview_enabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingView)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstandingView(is_semantic_preview_enabled);
                }
                int index = 0;
                foreach (bool toggle in SceneObjectToggle)
                {
                    bool _toggle = GUILayout.Toggle(toggle, "View/Export " + SceneObjectName[index] + " (" + ObjectColor[index] + ")");
                    if (_toggle != toggle)
                    {
                        SceneObjectToggle[index] = _toggle;
                        switch (SceneObjectName[index])
                        {
                            case "Bed":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.BED, 10, _toggle);
                                break;
                            case "Ceiling":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.CEILING, 10, _toggle);
                                break;
                            case "Chair":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.CHAIR, 10, _toggle);
                                break;
                            case "Floor":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.FLOOR, 10, _toggle);
                                break;
                            case "Table":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.TABLE, 10, _toggle);
                                break;
                            case "Wall":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.WALL, 10, _toggle);
                                break;
                            case "Window":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.WINDOW, 10, _toggle);
                                break;
                            case "Monitor":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.MONITOR, 10, _toggle);
                                break;
                            case "Furniture":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.FURNITURE, 10, _toggle);
                                break;
                            case "Door":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.DOOR, 10, _toggle);
                                break;
                            case "Picture":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.PICTURE, 10, _toggle);
                                break;
                            case "Person":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.PERSON, 10, _toggle);
                                break;
                            case "Light":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.LIGHT, 10, _toggle);
                                break;
                            case "Plant":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.PLANT, 10, _toggle);
                                break;
                            case "Curtain":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.CURTAIN, 10, _toggle);
                                break;
                            case "Pillow":
                                ViveSR_SceneUnderstanding.SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType.PILLOW, 10, _toggle);
                                break;
                        }

                    }
                    index++;
                }
            }

            if (ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding && ViveSR_RigidReconstruction.IsScanning)
            {

                if (GUILayout.Button("Export SceneObjects (.xml)", GUILayout.ExpandWidth(false)))
                {
                    ViveSR_SceneUnderstanding.StartExporting(SemanticObjDir);
                }
            }
            GUILayout.Label(new GUIContent("--SceneObjects Operation--"), style_bold);
            if (GUILayout.Button("Load & Show SceneObjects BoundingBox", GUILayout.ExpandWidth(false)))
            {
                SemanticObjDirPath = ReconstructionResultDir + ReconsSceneDir + SemanticObjDir;
                ViveSR_SceneUnderstanding.ImportSceneObjects(SemanticObjDirPath);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CHAIR, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CEILING, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.FLOOR, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.WALL, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.BED, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.TABLE, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.MONITOR, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.WINDOW, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.FURNITURE, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.DOOR, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PICTURE, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PERSON, true, true);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.LIGHT, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PLANT, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CURTAIN, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PILLOW, true, false);
            }
            if (GUILayout.Button("Destroy All SceneObjects BoundingBox", GUILayout.ExpandWidth(false)))
            {
                ViveSR_SceneUnderstanding.DestroySceneObjects();
            }

            if (ViveSR_RigidReconstruction.IsExporting || ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo)
            {
                if (ViveSR_RigidReconstruction.IsExporting) GUILayout.Label(new GUIContent("Exporting reconstruction model..."), style_bold);
                if (ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo) GUILayout.Label(new GUIContent("Exporting scene understanding model..."), style_bold);
                if (GUILayout.Button("Stop Export All Model"))
                {
                    ViveSR_RigidReconstruction.TerminateExporting();
                    ViveSR_RigidReconstruction.ResetReconstructionModule();
                }
            }
        }
        #endregion
    }
}
