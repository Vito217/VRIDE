using UnityEngine;
using UnityEditor;
using Vive.Plugin.SR;

[CustomEditor(typeof(ViveSR_DualCameraImageRenderer))]
[CanEditMultipleObjects]
public class ViveSR_DualCameraImageRendererEditor : Editor
{
    float near_occlusion_distThres;
    float far_occlusion_dist_thres;
    float near_collider_dist_thres;
    float far_collider_dist_thres;
    string[] undistort_method = new[] { "Undistorted By Mesh", "Undistorted By SRModule" };
    string[] depth_cause_mode = new[] { "DEFAULT", "CLOSE_RANGE"};
    bool set_seethrough_fps = false;
    float seethrough_fps = 60.0f;
    bool set_depth_fps = false;
    float depth_fps = 30.0f;
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();

        if (!Application.isPlaying) return;

        GUIStyle style = new GUIStyle();
        style.fontStyle = FontStyle.Bold;

        GUILayout.BeginHorizontal();
        GUILayout.Label("Undistort Method:");
        int cur_mode = (int)ViveSR_DualCameraImageRenderer.UndistortMethod;
        cur_mode = EditorGUILayout.Popup(cur_mode, undistort_method);
        GUILayout.EndHorizontal();
        if (cur_mode != (int)ViveSR_DualCameraImageRenderer.UndistortMethod)
        {
            ViveSR_DualCameraImageRenderer.UndistortMethod = (UndistortionMethod)cur_mode;
        }

        ViveSR_DualCameraImageRenderer.UpdateDistortedMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateDistortedMaterial, "Update Camera Material");
        ViveSR_DualCameraImageRenderer.UpdateUndistortedMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateUndistortedMaterial, "Update Undistorted Material");
        ViveSR_DualCameraImageRenderer.UpdateDepthMaterial = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.UpdateDepthMaterial, "Update Depth Material");

        GUILayout.Label(new GUIContent("[FPS Setting]"), style);
        set_seethrough_fps = GUILayout.Toggle(set_seethrough_fps, "Set See-through FPS");
        if (set_seethrough_fps) {
            GUILayout.Box("Value: " + (int)Mathf.Round(seethrough_fps));
            float NewSeethroughFPS = GUILayout.HorizontalSlider(seethrough_fps, 1.0f, 60.0f);
            if (NewSeethroughFPS != seethrough_fps) {
                SRWorkModule_API.SetPassThroughMaxFps((int)Mathf.Round(NewSeethroughFPS));
                seethrough_fps = NewSeethroughFPS;
            }
        }

        GUILayout.Label(new GUIContent("[Depth Setting]"), style);
        string btn_str_enable_depth_process = ViveSR_DualCameraImageCapture.IsDepthProcessing ? "Disable Depth Processing" : "Enable Depth Processing";
        if (GUILayout.Button(btn_str_enable_depth_process))
        {
            ViveSR_DualCameraImageCapture.SetDepthCase(DepthCase.CLOSE_RANGE);
            ViveSR_DualCameraImageCapture.EnableDepthProcess(!ViveSR_DualCameraImageCapture.IsDepthProcessing);
        }

        if (ViveSR_DualCameraImageCapture.IsDepthProcessing)
        {
            GUILayout.BeginHorizontal();
            GUILayout.Label("Depth Case     :");
            int curDepthCase = (int)ViveSR_DualCameraImageCapture.DepthCase;
            curDepthCase = EditorGUILayout.Popup(curDepthCase, depth_cause_mode);
            GUILayout.EndHorizontal();
            if (curDepthCase != (int)ViveSR_DualCameraImageCapture.DepthCase)
            {
                ViveSR_DualCameraImageCapture.SetDepthCase((DepthCase)curDepthCase);
            }
            EditorGUILayout.Separator();
            ViveSR_DualCameraImageCapture.IsDepthRefinementEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsDepthRefinementEnabled, "Enable Depth Refinement");
            ViveSR_DualCameraImageCapture.IsDepthEdgeEnhanceEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsDepthEdgeEnhanceEnabled, "Enable Depth Edge Enhance");

            EditorGUILayout.Separator();
            ViveSR_DualCameraImageRenderer.DepthImageOcclusion = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.DepthImageOcclusion, "Enable Depth Occlusion");
            ViveSR_DualCameraImageRenderer.VisualizeDepthOcclusion = GUILayout.Toggle(ViveSR_DualCameraImageRenderer.VisualizeDepthOcclusion, "Visualize Depth Occlusion");
            {
                GUILayout.Label("Near Occlusion Distance:");
                GUILayout.BeginHorizontal();
                near_occlusion_distThres = ViveSR_DualCameraImageRenderer.OcclusionNearDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraImageRenderer.OcclusionNearDistance, 0.05f, 10.0f);
                GUILayout.Label("" + near_occlusion_distThres.ToString("0.00") + "m");
                GUILayout.EndHorizontal();

                GUILayout.Label("Far Occlusion Distance:");
                GUILayout.BeginHorizontal();
                far_occlusion_dist_thres = ViveSR_DualCameraImageRenderer.OcclusionFarDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraImageRenderer.OcclusionFarDistance, 0.05f, 10.0f);
                GUILayout.Label("" + far_occlusion_dist_thres.ToString("0.00") + "m");
                GUILayout.EndHorizontal();
            }

            EditorGUILayout.Separator();
            ViveSR_DualCameraDepthCollider.UpdateDepthCollider = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.UpdateDepthCollider, "Update Depth Collider");
            if (ViveSR_DualCameraDepthCollider.UpdateDepthCollider)
            {
                ViveSR_DualCameraDepthCollider.DepthColliderVisibility = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.DepthColliderVisibility, "Show Depth Collider");

                ViveSR_DualCameraDepthCollider.UpdateDepthColliderHoleFilling = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.UpdateDepthColliderHoleFilling, "Enable Depth Hole Filling");

                ViveSR_DualCameraDepthCollider.UpdateDepthColliderRange = GUILayout.Toggle(ViveSR_DualCameraDepthCollider.UpdateDepthColliderRange, "Adjust Depth Collider Clip Distance");


                if (ViveSR_DualCameraDepthCollider.UpdateDepthColliderRange)
                {
                    GUILayout.Label("Depth Collider Clip Distance - Near:");
                    GUILayout.BeginHorizontal();
                    near_collider_dist_thres = ViveSR_DualCameraDepthCollider.UpdateColliderNearDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraDepthCollider.UpdateColliderNearDistance, 0.0f, 10.0f);
                    GUILayout.Label("" + near_collider_dist_thres.ToString("0.00") + "m");
                    GUILayout.EndHorizontal();

                    GUILayout.Label("Depth Collider Clip Distance - Far:");
                    GUILayout.BeginHorizontal();
                    far_collider_dist_thres = ViveSR_DualCameraDepthCollider.UpdateColliderFarDistance = GUILayout.HorizontalSlider(ViveSR_DualCameraDepthCollider.UpdateColliderFarDistance, 0.0f, 10.0f);
                    GUILayout.Label("" + far_collider_dist_thres.ToString("0.00") + "m");
                    GUILayout.EndHorizontal();
                }
            }
            GUILayout.Label(new GUIContent("[FPS Setting]"), style);
            set_depth_fps = GUILayout.Toggle(set_depth_fps, "Set Depth FPS");
            if (set_depth_fps) {
                GUILayout.Box("Value: " + (int)Mathf.Round(depth_fps));
                float NewDepthFPS = GUILayout.HorizontalSlider(depth_fps, 1.0f, 60.0f);
                if (NewDepthFPS != depth_fps) {
                    SRWorkModule_API.SetDepthMaxFps((int)Mathf.Round(NewDepthFPS));
                    depth_fps = NewDepthFPS;
                }
            }
            EditorGUILayout.Separator();
            ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled = GUILayout.Toggle(ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled, "Start Recording Dataset");
            if (ViveSR_DualCameraImageCapture.IsRecordingDatasetEnabled)
            {
                GUILayout.Label(new GUIContent("Recording Dataset..."), style);
            }
        }
    }
}