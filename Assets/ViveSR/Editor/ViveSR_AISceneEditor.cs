using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using Vive.Plugin.SR;
[CustomEditor(typeof(ViveSR_AIScene))]
public class ViveSR_AISceneEditor : Editor {

    SegmentWay SegmentMethod;//= SegmentWay.AI_SCENE;
    SegmentWay PreviousSegmentMethod;
    float MaxDistance = 200.0f;
    float MinDistance = 40.0f;
    Color BGColor = Color.green;
    public override void OnInspectorGUI()
    {
        serializedObject.Update();
        DrawDefaultInspector();

        if (!Application.isPlaying) return;

        GUIStyle style = new GUIStyle();
        style.fontStyle = FontStyle.Bold;

        string btn_str_enable_human_cut_process = ViveSR_AIScene.IsHumanCutProcessing ? "Disable Human Cut Processing" : "Enable Human Cut Processing";
        if (GUILayout.Button(btn_str_enable_human_cut_process))
        {
            ViveSR_AIScene.Instance.EnableHumanCutProcess(!ViveSR_AIScene.IsHumanCutProcessing);
            PreviousSegmentMethod = ViveSR_AIScene.Instance.AISegmentPlaneLeft.GetSegmentMethod();

            if (!ViveSR_AIScene.Initial)
            {
                ViveSR_AIScene.Instance.AISegmentPlaneLeft.initial(PreviousSegmentMethod);
                ViveSR_AIScene.Initial = true;
            }

            switch (PreviousSegmentMethod)
            {
                case SegmentWay.AI_SCENE:
                    ViveSR_AIScene.Instance.EnableAISceneProcess(!ViveSR_AIScene.IsAISceneProcessing);
                    break;
                case SegmentWay.DEPTH:
                    break;
                case SegmentWay.BACKGROUND_COLOR:
                    break;
            }
        }
        if (ViveSR_AIScene.IsHumanCutProcessing)
        {
            GUILayout.BeginHorizontal();
            PreviousSegmentMethod = ViveSR_AIScene.Instance.AISegmentPlaneLeft.GetSegmentMethod();
            SegmentMethod = (SegmentWay)EditorGUILayout.EnumPopup("Segment Way", PreviousSegmentMethod);
            if (PreviousSegmentMethod != SegmentMethod)
            {
                switch (PreviousSegmentMethod)
                {
                    case SegmentWay.AI_SCENE:
                        ViveSR_AIScene.Instance.EnableAISceneProcess(!ViveSR_AIScene.IsAISceneProcessing);
                        break;
                    case SegmentWay.DEPTH:
                        break;
                    case SegmentWay.BACKGROUND_COLOR:
                        break;
                }
                switch (SegmentMethod)
                {
                    case SegmentWay.AI_SCENE:
                        ViveSR_AIScene.Instance.EnableAISceneProcess(!ViveSR_AIScene.IsAISceneProcessing);
                        break;
                    case SegmentWay.DEPTH:
                        break;
                    case SegmentWay.BACKGROUND_COLOR:
                        break;
                }
            }
            ViveSR_AIScene.Instance.AISegmentPlaneLeft.SetSegmentMethod(SegmentMethod);
            GUILayout.EndHorizontal();
            switch(SegmentMethod)
            {
                case SegmentWay.AI_SCENE:
                    break;
                case SegmentWay.DEPTH:
                    ViveSR_AIScene.Instance.AISegmentPlaneLeft.GetMaxMinDistance(out MaxDistance, out MinDistance);
                    GUILayout.BeginHorizontal();
                    GUILayout.Label("Min Distance:");
                    MinDistance = GUILayout.HorizontalSlider(MinDistance, 0.0f, 300.0f);
                    GUILayout.Label("" + MinDistance.ToString("0") + "cm");
                    GUILayout.EndHorizontal();
                    GUILayout.BeginHorizontal();
                    GUILayout.Label("Max Distance:");
                    MaxDistance = GUILayout.HorizontalSlider(MaxDistance, 0.0f, 300.0f);
                    GUILayout.Label("" + MaxDistance.ToString("0") + "cm");
                    GUILayout.EndHorizontal();
                    ViveSR_AIScene.Instance.AISegmentPlaneLeft.SetMaxMinDistance(MaxDistance, MinDistance);
                    break;
                case SegmentWay.BACKGROUND_COLOR:
                    ViveSR_AIScene.Instance.AISegmentPlaneLeft.GetBackgroundColor(out BGColor);
                    BGColor = EditorGUILayout.ColorField("Background Color", BGColor);
                    ViveSR_AIScene.Instance.AISegmentPlaneLeft.SetBackgroundColor(BGColor);
                    break;
            }
        }
        serializedObject.ApplyModifiedProperties();
    }
}