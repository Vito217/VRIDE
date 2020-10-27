using UnityEditor;
using UnityEngine;
using Vive.Plugin.SR;
using System.Collections.Generic;

[CustomEditor(typeof(ViveSR_RigidReconstructionRenderer))]
[CanEditMultipleObjects]
public class ViveSR_RigidReconstructionRendererEditor : Editor
{
    string[] display_mode = new[] { "Full Scene Point", "Field Of View", "Adaptive Mesh" };
    string[] adaptive_lable = new[] { "64cm", "32cm", "16cm", "8cm", "4cm", "2cm" };
    List<float> adaptive_level = new List<float> { 64.0f, 32.0f, 16.0f, 8.0f, 4.0f, 2.0f };
    int max_select_id, min_select_id;
    float error_thres, export_max_size, export_min_size;

    bool[] scene_object_toggle = new[] { false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false};
    string[] scene_object_name = new[] { "Floor", "Wall", "Ceiling", "Chair", "Table", "Bed", "Monitor", "Window", "Furniture", "Door", "Picture", "Person", "Light", "Plant", "Curtain","Pillow" };
    string[] object_color = new[] { "Green", "Blue", "Yellow", "Red", "Cyan", "Magenta", "Dark Red", "Dark Green", "Purple", "Pink", "Orange", "Grass Green", "Light yellow", "Mint Green", "Dark brown", "Dark blue"};

    public string recons_scene_dir = "Recons3DAsset/";
    public string semantic_obj_dir = "SemanticIndoorObj/";
    private string reconstruction_result_dir = System.IO.Path.GetDirectoryName(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData)) + "\\LocalLow\\HTC Corporation\\SR_Reconstruction_Output\\";

    bool set_reconstruction_fps = false;
    float reconstruction_fps = 10.0f;

    string[] qualityList = new[] { "LOW - 4cm", "MID - 2cm", "HIGH - 1cm"};
    int selectQuality = 1;

    bool set_voxel_size = false;
    float geometrySize = 0.02f;
    float colorSize = 0.02f;

    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();

        if (!Application.isPlaying) return;

        EditorGUILayout.Separator();
        EditorGUILayout.Separator();
        GUIStyle style = new GUIStyle();
        style.fontStyle = FontStyle.Bold;
        GUILayout.Label(new GUIContent("[Runtime Command]"), style);
        EditorGUILayout.Separator();

        string btn_str_enable_reconstruction_process = ViveSR_RigidReconstruction.ReconstructionProcessing ? "Disable Reconstruction Processing" : "Enable Reconstruction Processing";
        if (GUILayout.Button(btn_str_enable_reconstruction_process))
        {
            ViveSR_RigidReconstruction.EnableReconstructionProcess(!ViveSR_RigidReconstruction.ReconstructionProcessing);
        }

        if (ViveSR_RigidReconstruction.ReconstructionProcessing)
        {
            GUILayout.Label(new GUIContent("[FPS Setting]"), style);
            set_reconstruction_fps = GUILayout.Toggle(set_reconstruction_fps, "Set Reconstruction FPS");
            if (set_reconstruction_fps)
            {
                GUILayout.Box("Value: " + (int)Mathf.Round(reconstruction_fps));
                float NewReconstructionFPS = GUILayout.HorizontalSlider(reconstruction_fps, 1.0f, 60.0f);
                if (NewReconstructionFPS != reconstruction_fps)
                {
                    SRWorkModule_API.SetReconstructionMaxFps((int)Mathf.Round(NewReconstructionFPS));
                    reconstruction_fps = NewReconstructionFPS;
                }
            }

            // Quality setting
            if (!ViveSR_RigidReconstruction.IsScanning)
            {
                set_voxel_size = GUILayout.Toggle(set_voxel_size, "Set Voxel Size");
                if (set_voxel_size)
                {
                    selectQuality = EditorGUILayout.Popup(selectQuality, qualityList);
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

            // start / stop
            GUILayout.Label(new GUIContent("--Start/Stop--"), style);
            if (!ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview || ViveSR_RigidReconstruction.IsScannedMeshPreviewCompleted)
            {
                if (GUILayout.Button("Start Reconstruction"))
                {
                    ViveSR_RigidReconstruction.StartScanning();
                }
            }

            if (ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
            {
                if (GUILayout.Button("Stop Reconstruction"))
                {
                    ViveSR_RigidReconstruction.StopScanning();
                }

                // live extraction mode
                EditorGUILayout.Separator();
                GUILayout.Label(new GUIContent("--Live Extraction--"), style);
                int cur_mode = (int)ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode;
                GUILayout.BeginHorizontal();
                GUILayout.Label("Display Mode:");
                cur_mode = EditorGUILayout.Popup(cur_mode, display_mode);
                GUILayout.EndHorizontal();

                bool enableSector = GUILayout.Toggle(ViveSR_RigidReconstructionRenderer.EnableSector, "Enable Sectioned Mesh");
                if (enableSector != ViveSR_RigidReconstructionRenderer.EnableSector) ViveSR_RigidReconstructionRenderer.EnableSector = enableSector;

                int sectorGroupNum = EditorGUILayout.IntSlider("Sectioned Mesh Limit", ViveSR_RigidReconstructionRenderer.MaxActiveGO, 50, 500);
                if (sectorGroupNum != ViveSR_RigidReconstructionRenderer.MaxActiveGO) ViveSR_RigidReconstructionRenderer.MaxActiveGO = sectorGroupNum;

                if (cur_mode != (int)ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode)
                {
                    ViveSR_RigidReconstructionRenderer.LiveMeshDisplayMode = (ReconstructionDisplayMode)cur_mode;
                }
                // adaptive tunning
                if (cur_mode == (int)ReconstructionDisplayMode.ADAPTIVE_MESH)
                {
                    EditorGUILayout.Separator();
                    GUILayout.Label(new GUIContent("--Live Adaptive Mesh Tuning--"), style);
                    DrawAdaptiveParamUI(ViveSR_RigidReconstruction.LiveAdaptiveMaxGridSize, ViveSR_RigidReconstruction.LiveAdaptiveMinGridSize, ViveSR_RigidReconstruction.LiveAdaptiveErrorThres);
                    ViveSR_RigidReconstruction.LiveAdaptiveMaxGridSize = adaptive_level[max_select_id];
                    ViveSR_RigidReconstruction.LiveAdaptiveMinGridSize = adaptive_level[min_select_id];
                    ViveSR_RigidReconstruction.LiveAdaptiveErrorThres = error_thres;
                }
            }

            // export
            EditorGUILayout.Separator();
            if (ViveSR_RigidReconstruction.IsScanning && !ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview)
            {
                GUILayout.Label(new GUIContent("--Export--"), style);
                bool export_adaptive = ViveSR_RigidReconstruction.ExportAdaptiveMesh;
                ViveSR_RigidReconstruction.ExportAdaptiveMesh = GUILayout.Toggle(export_adaptive, "Export Adaptive Model");

                if (ViveSR_RigidReconstruction.ExportAdaptiveMesh)
                {
                    // live extraction mode
                    EditorGUILayout.Separator();
                    GUILayout.Label(new GUIContent("--Export Adaptive Mesh Tuning--"), style);
                    DrawAdaptiveParamUI(ViveSR_RigidReconstruction.ExportAdaptiveMaxGridSize, ViveSR_RigidReconstruction.ExportAdaptiveMinGridSize, ViveSR_RigidReconstruction.ExportAdaptiveErrorThres);
                    ViveSR_RigidReconstruction.ExportAdaptiveMaxGridSize = adaptive_level[max_select_id];
                    ViveSR_RigidReconstruction.ExportAdaptiveMinGridSize = adaptive_level[min_select_id];
                    ViveSR_RigidReconstruction.ExportAdaptiveErrorThres = error_thres;
                }

                // only support adaptive mesh now
                if (GUILayout.Button("Preview Scanned Model"))
                {
                    ViveSR_RigidReconstruction.ExtractModelPreviewData();
                }

            }
            if (!ViveSR_RigidReconstruction.IsExporting && !ViveSR_RigidReconstruction.IsDuringScannedMeshPreview && (ViveSR_RigidReconstruction.IsScanning != ViveSR_RigidReconstruction.IsScannedMeshPreviewCompleted))
            {
                if (GUILayout.Button("Start Export Model"))
                {
                    ViveSR_RigidReconstruction.StopScanning();
                    ViveSR_RigidReconstruction.StartExporting("Model");
                }
            }

            // Scene Understanding 
            // output surrounding objects of interest and their attributes 
            #region Scene Understanding
            EditorGUILayout.Separator();

            GUILayout.Label(new GUIContent("--Scene Understanding--"), style);
            if (ViveSR_RigidReconstruction.IsScanning)
            {
                bool isSemanticEnabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding, "Enable Scene Understanding");
                if (isSemanticEnabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstanding(isSemanticEnabled, false);
                }
            }
            if (ViveSR_SceneUnderstanding.IsEnabledSceneUnderstanding && ViveSR_RigidReconstruction.IsScanning)
            {
                bool isSemanticRefinementEnabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingRefinement, "Enable Scene Understanding Refinement");
                if (isSemanticRefinementEnabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingRefinement)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstandingRefinement(isSemanticRefinementEnabled);
                }
                bool isSemanticPreviewEnabled = GUILayout.Toggle(ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingView, "Enable Preview");
                if (isSemanticPreviewEnabled != ViveSR_SceneUnderstanding.IsEnabledSceneUnderstandingView)
                {
                    ViveSR_SceneUnderstanding.EnableSceneUnderstandingView(isSemanticPreviewEnabled);
                }
                int index = 0;
                foreach (bool toggle in scene_object_toggle)
                {
                    bool _toggle = GUILayout.Toggle(toggle, "View/Export " + scene_object_name[index] + " (" + object_color[index] + ")");
                    if (_toggle != toggle)
                    {
                        scene_object_toggle[index] = _toggle;
                        switch (scene_object_name[index])
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

                if (GUILayout.Button("Export SceneObjects (.xml)"))
                {
                    ViveSR_SceneUnderstanding.StartExporting(semantic_obj_dir);
                }
            }
            if (GUILayout.Button("Load & Show SceneObjects BoundingBox"))
            {
                ViveSR_SceneUnderstanding.ImportSceneObjects(reconstruction_result_dir + recons_scene_dir + semantic_obj_dir);

                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CHAIR, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CEILING, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.FLOOR, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.WALL, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.BED, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.TABLE, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.MONITOR, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.WINDOW, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.FURNITURE, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.DOOR, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PICTURE, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PERSON, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.LIGHT, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PLANT, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.CURTAIN, true, false);
                ViveSR_SceneUnderstanding.ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType.PILLOW, true, false);
            }

            if (GUILayout.Button("Destroy All SceneObjects BoundingBox"))
            {
                ViveSR_SceneUnderstanding.DestroySceneObjects();
            }
        }
        #endregion
        if (ViveSR_RigidReconstruction.IsExporting || ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo)
        {
            if (ViveSR_RigidReconstruction.IsExporting) GUILayout.Label(new GUIContent("Exporting reconstruction model..."), style);
            if (ViveSR_SceneUnderstanding.IsExportingSceneUnderstandingInfo) GUILayout.Label(new GUIContent("Exporting scene understanding model..."), style);
            if (GUILayout.Button("Stop Export All Model"))
            {
                ViveSR_RigidReconstruction.TerminateExporting();
                ViveSR_RigidReconstruction.ResetReconstructionModule();
            }
        }
    }

    private void DrawAdaptiveParamUI(float maxGridSize, float minGridSize, float thres)
    {
        GUILayout.Label("Adaptive Range (Max~Min):");
        GUILayout.BeginHorizontal();
        max_select_id = adaptive_level.IndexOf(maxGridSize);
        min_select_id = adaptive_level.IndexOf(minGridSize);
        max_select_id = EditorGUILayout.Popup(max_select_id, adaptive_lable);
        min_select_id = EditorGUILayout.Popup(min_select_id, adaptive_lable);
        GUILayout.EndHorizontal();

        GUILayout.Label("Divide Threshold:");
        GUILayout.BeginHorizontal();
        error_thres = GUILayout.HorizontalSlider(thres, 0.0f, 1.5f);
        GUILayout.Label("" + error_thres.ToString("0.00"));
        GUILayout.EndHorizontal();
    }
}
