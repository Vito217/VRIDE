#if UNITY_EDITOR
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEditor;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_Settings : EditorWindow
    {
        private static ViveSR_Settings Window;
        private static List<Page> SettingPages = new List<Page>();
        private static int CountPage;
        private static int CurrentPage;

        public static void Update()
        {
            CurrentPage = CountPage = 0;
            SettingPages.Clear();

            Page[] pages = new Page[]
            {
                (Page_VRSetting)CreateInstance(typeof(Page_VRSetting)),
                (Page_LayerSetting)CreateInstance(typeof(Page_LayerSetting)),
                //(Page_QualitySetting)CreateInstance(typeof(Page_QualitySetting)),
                (Page_Finish)CreateInstance(typeof(Page_Finish)),
            };
            foreach (var page_iter in pages)
            {
                if (page_iter.IsNeedShow()) SettingPages.Add(page_iter);
            }
            CountPage = SettingPages.Count;
            if (CountPage > 1)
            {
                Window = GetWindow<ViveSR_Settings>(true);
                Window.minSize = new Vector2(300, 400);
            }
            EditorApplication.update -= Update;
        }

        void OnGUI()
        {
            GUI.skin.label.wordWrap = true;
            var rect = GUILayoutUtility.GetRect(position.width, 150, GUI.skin.box);
            var logo = AssetDatabase.LoadAssetAtPath<Texture2D>("Assets\\ViveSR\\Textures\\SRWorks_logo.png");
            if (logo) GUI.DrawTexture(rect, logo, ScaleMode.ScaleToFit);

            EditorGUI.ProgressBar(new Rect(0, 157.5f, position.width, 20), (float)(CurrentPage + 1) / CountPage, "");
            GUILayout.Label("Page : " + (CurrentPage + 1) + " / " + CountPage, EditorStyles.boldLabel);
            GUILayout.Space(5);

            for (int p = CurrentPage; p < CountPage; p++)
            {
                if (SettingPages[p].IsNeedShow())
                {

                    SettingPages[p].RenderGUI();
                    break;
                }
                else
                    ++CurrentPage;
            }
            if (CurrentPage == CountPage || ViveSR_DualCameraRig.Instance == null) Close();
        }


        public abstract class Page : EditorWindow
        {
            public abstract string Name { get; }
            public abstract bool IsNeedShow();
            public abstract void RenderGUI();
        }
    }

    public class Page_VRSetting : ViveSR_Settings.Page
    {
        public override string Name { get { return "VR Setting"; } }

        const string HelpboxText_RemindEnableOpenVRSupport = "Enable OpenVR support?";
        public bool AutoEnableVR
        {
            get { return EditorPrefs.GetBool("ViveSR_AutoEnableVR", true); }
            set { EditorPrefs.SetBool("ViveSR_AutoEnableVR", value); }
        }

        public override bool IsNeedShow()
        {
            return IsNeedOpenVRSupport();
        }

        public override void RenderGUI()
        {
            EditorGUILayout.HelpBox(HelpboxText_RemindEnableOpenVRSupport, MessageType.Warning);
            GUILayout.FlexibleSpace();
            GUILayout.BeginHorizontal();
            if (GUILayout.Button("Ignore")) AutoEnableVR = false;
            if (GUILayout.Button("Accept"))
            {
                EnableOpenVRSupport();
                return;
            }
            GUILayout.EndHorizontal();
        }

        private bool IsNeedOpenVRSupport()
        {
            //if (!AutoEnableVR) return false;
            if (!PlayerSettings.virtualRealitySupported) return true;

#if (UNITY_5_4 || UNITY_5_3 || UNITY_5_2 || UNITY_5_1 || UNITY_5_0)
			var devices = UnityEditorInternal.VR.VREditor.GetVREnabledDevices(BuildTargetGroup.Standalone);
#elif (UNITY_2017 || UNITY_2017_1_OR_NEWER)
            var devices = UnityEngine.XR.XRSettings.supportedDevices;
#else
            var devices = UnityEditorInternal.VR.VREditor.GetVREnabledDevicesOnTargetGroup(BuildTargetGroup.Standalone);
#endif
            foreach (var device in devices)
            {
                if (device.ToLower() == "openvr") { return false; }
            }

            return true;
        }

        private void EnableOpenVRSupport()
        {
            PlayerSettings.virtualRealitySupported = true;

            if (IsNeedOpenVRSupport())
            {
                UnityEditor.PlayerSettings.SetVirtualRealitySupported(BuildTargetGroup.Standalone, true);
            }
        }
    }

    public class Page_LayerSetting : ViveSR_Settings.Page
    {
        public override string Name { get { return "Layer Setting"; } }
        private const int VRWorldLayer = 2;
        private const int ReconsMeshLayer = 3;
        private const int RenderPlaneLeftLayer = 4;
        private const int RenderPlaneRightLayer = 5;
        private bool IsNeedStereoSetting = false;
        private bool IsNeedPortalSetting = false;
        private bool IsNeedReconsMeshSetting = false;
        private int[] candidateLayerIndex = new int[6];
        private bool IsStereoInputValid = false;
        private bool IsPortalInputValid = false;
        private bool IsReconsMeshInputValid = false;

        const string Label_LayerSetting = "ViveSR needs four layers for rendering the camera images.";
        const string HelpboxText_LayerNotValid = "Please insert valid values";
        const string BtnName_UpdateLayers = "Update layers";
        const string DialogTitle_Error = "Error";
        const string DialogTitle_ScriptNotFound = "ViveSR_DualCameraRig.Instance does not exist in current scene";
        const string Label_LayerChangeName = "If you want to change layer name, please modify the values in ViveSR_Layers.cs.";
        const string Label_PortalLayerSetting = "ViveSR needs 1 layer for rendering portal effect.";
        const string Label_ReconsMeshLayerSetting = "ViveSR needs 1 layer for rendering reconstructed mesh.";

        /// <summary>
        /// Store the layer numbers in string.
        /// </summary>
        private string[] CandidateLayername = new string[6] { "", "", "", "", "", "" };
        private Rect LastRect;

        public override bool IsNeedShow()
        {
            IsNeedPortalSetting = FindObjectOfType<ViveSR_PortalManager>() != null && !CheckLayerByName(ViveSR_Layers.VirtualWorldLayerName);
            IsNeedReconsMeshSetting = ViveSR_DualCameraRig.Instance != null && !CheckLayerByName(ViveSR_RigidReconstruction.ReconsMeshLayerName);
            IsNeedStereoSetting = !CheckLayers(DualCameraIndex.LEFT) || !CheckLayers(DualCameraIndex.RIGHT) ||
                !CheckLayerByName(ViveSR_Layers.RenderPlaneLeftLayerName) || !CheckLayerByName(ViveSR_Layers.RenderPlaneRightLayerName);
            return IsNeedStereoSetting || IsNeedPortalSetting || IsNeedReconsMeshSetting;
        }

        public override void RenderGUI()
        {
            FindCandidate();

            if (IsNeedStereoSetting) OnGUIStereo();

            // portal layer part
            if (IsNeedPortalSetting) OnGUIPortal();

            if (IsNeedReconsMeshSetting) OnGUIReconsMesh();

            if (IsStereoInputValid || IsPortalInputValid || IsReconsMeshInputValid)
                if (GUILayout.Button(BtnName_UpdateLayers))
                {
                    if (ViveSR_DualCameraRig.Instance != null)
                    {
                        if (IsStereoInputValid) SetStereoLayer();
                        if (IsPortalInputValid) SetPortalLayer();
                        if (IsReconsMeshInputValid) SetReconsMeshLayer();
                        //EditorUtility.DisplayDialog(BtnName_UpdateLayers, "Done!", "Ok");
                    }
                    else
                    {
                        EditorUtility.DisplayDialog(DialogTitle_Error, DialogTitle_ScriptNotFound, "Cancel");
                    }
                }
        }

        private void OnGUIStereo()
        {
            GUILayout.Label(Label_LayerSetting);
            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_Layers.DualCameraLeftLayerName + ":  ");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[(int)DualCameraIndex.LEFT] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[(int)DualCameraIndex.LEFT], 2);
            CandidateLayername[(int)DualCameraIndex.LEFT] = Regex.Replace(CandidateLayername[(int)DualCameraIndex.LEFT], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_Layers.DualCameraRightLayerName + ":");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[(int)DualCameraIndex.RIGHT] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[(int)DualCameraIndex.RIGHT], 2);
            CandidateLayername[(int)DualCameraIndex.RIGHT] = Regex.Replace(CandidateLayername[(int)DualCameraIndex.RIGHT], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_Layers.RenderPlaneLeftLayerName + ":");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[RenderPlaneLeftLayer] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[RenderPlaneLeftLayer], 2);
            CandidateLayername[RenderPlaneLeftLayer] = Regex.Replace(CandidateLayername[RenderPlaneLeftLayer], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_Layers.RenderPlaneRightLayerName + ":");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[RenderPlaneRightLayer] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[RenderPlaneRightLayer], 2);
            CandidateLayername[RenderPlaneRightLayer] = Regex.Replace(CandidateLayername[RenderPlaneRightLayer], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            System.Int32.TryParse(CandidateLayername[(int)DualCameraIndex.LEFT], out candidateLayerIndex[(int)DualCameraIndex.LEFT]);
            System.Int32.TryParse(CandidateLayername[(int)DualCameraIndex.RIGHT], out candidateLayerIndex[(int)DualCameraIndex.RIGHT]);
            System.Int32.TryParse(CandidateLayername[RenderPlaneLeftLayer], out candidateLayerIndex[RenderPlaneLeftLayer]);
            System.Int32.TryParse(CandidateLayername[RenderPlaneRightLayer], out candidateLayerIndex[RenderPlaneRightLayer]);

            GUILayout.FlexibleSpace();
            if (CandidateLayername[(int)DualCameraIndex.LEFT] != CandidateLayername[(int)DualCameraIndex.RIGHT] &&
                candidateLayerIndex[(int)DualCameraIndex.LEFT] >= 8 &&
                candidateLayerIndex[(int)DualCameraIndex.LEFT] < 32 &&
                candidateLayerIndex[(int)DualCameraIndex.RIGHT] >= 8 &&
                candidateLayerIndex[(int)DualCameraIndex.RIGHT] < 32 &&
                candidateLayerIndex[RenderPlaneLeftLayer] >= 8 &&
                candidateLayerIndex[RenderPlaneLeftLayer] < 32 &&
                candidateLayerIndex[RenderPlaneRightLayer] >= 8 &&
                candidateLayerIndex[RenderPlaneRightLayer] < 32)
            {
                IsStereoInputValid = true;
            }
            else
                EditorGUILayout.HelpBox(HelpboxText_LayerNotValid, MessageType.Error);
            GUILayout.Label(Label_LayerChangeName);
        }

        private void OnGUIPortal()
        {
            GUILayout.Label(Label_PortalLayerSetting);
            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_Layers.VirtualWorldLayerName + ":");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[VRWorldLayer] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[VRWorldLayer], 2);
            CandidateLayername[VRWorldLayer] = Regex.Replace(CandidateLayername[VRWorldLayer], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            System.Int32.TryParse(CandidateLayername[VRWorldLayer], out candidateLayerIndex[VRWorldLayer]);

            GUILayout.FlexibleSpace();
            if (CandidateLayername[VRWorldLayer] != "" && candidateLayerIndex[VRWorldLayer] >= 8 && candidateLayerIndex[VRWorldLayer] < 32)
            {
                IsPortalInputValid = true;
            }
        }

        private void OnGUIReconsMesh()
        {
            GUILayout.Label(Label_ReconsMeshLayerSetting);
            GUILayout.BeginHorizontal();
            GUILayout.Label(ViveSR_RigidReconstruction.ReconsMeshLayerName + ":");
            LastRect = GUILayoutUtility.GetLastRect();
            CandidateLayername[ReconsMeshLayer] = GUI.TextField(new Rect(LastRect.x + 130, LastRect.y, 25, 20), CandidateLayername[ReconsMeshLayer], 2);
            CandidateLayername[ReconsMeshLayer] = Regex.Replace(CandidateLayername[ReconsMeshLayer], @"[^0-9.]", "");
            GUILayout.EndHorizontal();

            System.Int32.TryParse(CandidateLayername[ReconsMeshLayer], out candidateLayerIndex[ReconsMeshLayer]);

            GUILayout.FlexibleSpace();
            if (CandidateLayername[ReconsMeshLayer] != "" && candidateLayerIndex[ReconsMeshLayer] >= 8 && candidateLayerIndex[ReconsMeshLayer] < 32)
            {
                IsReconsMeshInputValid = true;
            }
        }

        private void SetStereoLayer()
        {
            ModifyLayer(System.Int32.Parse(CandidateLayername[(int)DualCameraIndex.LEFT]), ViveSR_Layers.DualCameraLeftLayerName);
            ModifyLayer(System.Int32.Parse(CandidateLayername[(int)DualCameraIndex.RIGHT]), ViveSR_Layers.DualCameraRightLayerName);
            ModifyLayer(System.Int32.Parse(CandidateLayername[RenderPlaneLeftLayer]), ViveSR_Layers.RenderPlaneLeftLayerName);
            ModifyLayer(System.Int32.Parse(CandidateLayername[RenderPlaneRightLayer]), ViveSR_Layers.RenderPlaneRightLayerName);
            UpdatePassThroCamLayers(DualCameraIndex.LEFT);
            UpdatePassThroCamLayers(DualCameraIndex.RIGHT);
            UpdateRenderCameraLayers();
        }

        private void SetPortalLayer()
        {
            ModifyLayer(System.Int32.Parse(CandidateLayername[VRWorldLayer]), ViveSR_Layers.VirtualWorldLayerName);
            UpdateCollisionMatrix();
        }

        private void SetReconsMeshLayer()
        {
            ModifyLayer(System.Int32.Parse(CandidateLayername[ReconsMeshLayer]), ViveSR_RigidReconstruction.ReconsMeshLayerName);
        }

        /// <summary>
        /// Find available user layers.
        /// </summary>
        private bool FindCandidate()
        {
            int[] candidate_layer = new int[6]
            {
                LayerMask.NameToLayer(ViveSR_Layers.DualCameraLeftLayerName),
                LayerMask.NameToLayer(ViveSR_Layers.DualCameraRightLayerName),
                LayerMask.NameToLayer(ViveSR_Layers.VirtualWorldLayerName),
                LayerMask.NameToLayer(ViveSR_RigidReconstruction.ReconsMeshLayerName),
                LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneLeftLayerName),
                LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneRightLayerName)
            };
            int search_layer = 31;
            for (int i = 0; i < candidate_layer.Length; i++)
            {
                if (candidate_layer[i] == -1)
                {
                    for (; search_layer > 8; search_layer--)
                    {
                        if (LayerMask.LayerToName(search_layer) == "")
                        {
                            bool occupied = false;
                            for (int j = i; j >= 0; j--) if (candidate_layer[j] == search_layer) occupied = true;
                            if (occupied) continue;

                            candidate_layer[i] = search_layer;
                            break;
                        }
                    }
                }
            }

            CandidateLayername[(int)DualCameraIndex.LEFT] = candidate_layer[(int)DualCameraIndex.LEFT].ToString();
            CandidateLayername[(int)DualCameraIndex.RIGHT] = candidate_layer[(int)DualCameraIndex.RIGHT].ToString();
            CandidateLayername[VRWorldLayer] = candidate_layer[VRWorldLayer].ToString();
            CandidateLayername[ReconsMeshLayer] = candidate_layer[ReconsMeshLayer].ToString();
            CandidateLayername[RenderPlaneLeftLayer] = candidate_layer[RenderPlaneLeftLayer].ToString();
            CandidateLayername[RenderPlaneRightLayer] = candidate_layer[RenderPlaneRightLayer].ToString();
            return true;
        }

        private bool CheckLayerByName(string layer_name)
        {
            int layer = LayerMask.NameToLayer(layer_name);
            return (layer != -1);
        }

        private bool CheckLayers(DualCameraIndex camera_index)
        {
            if (ViveSR_DualCameraRig.Instance == null || ViveSR_DualCameraRig.Instance == null) return false;
            string layername = camera_index == DualCameraIndex.LEFT ? ViveSR_Layers.DualCameraLeftLayerName : ViveSR_Layers.DualCameraRightLayerName;
            int mask = LayerMask.GetMask(layername);
            int layer = LayerMask.NameToLayer(layername);
            Camera cam = camera_index == DualCameraIndex.LEFT ?
                ViveSR_DualCameraRig.Instance.DualCameraLeft :
                ViveSR_DualCameraRig.Instance.DualCameraRight;
            ViveSR_TrackedCamera trackedCam = camera_index == DualCameraIndex.LEFT ?
                ViveSR_DualCameraRig.Instance.TrackedCameraLeft :
                ViveSR_DualCameraRig.Instance.TrackedCameraRight;

            // Check if the camera culling mask contains the specified mask.
            if ((cam.cullingMask & mask) == 0) return false;
            // Check object layers.
            if (trackedCam.gameObject.layer != layer) return false;
            if (trackedCam.Anchor.gameObject.layer != layer) return false;
            if (trackedCam.DepthImageOccluder.gameObject.layer != layer) return false;
            if (trackedCam.ImagePlane.gameObject.layer != layer) return false;
            return true;
        }

        private void UpdateCollisionMatrix()
        {
            Physics.IgnoreLayerCollision(LayerMask.NameToLayer("Default"), LayerMask.NameToLayer(ViveSR_Layers.VirtualWorldLayerName));
        }

        private void UpdatePassThroCamLayers(DualCameraIndex camera_index)
        {
            string layer_name = camera_index == DualCameraIndex.LEFT ? ViveSR_Layers.DualCameraLeftLayerName : ViveSR_Layers.DualCameraRightLayerName;
            // The dual camera culling mask contains the built-in layers and the dual camera left/right layer.
            int mask = LayerMask.GetMask(ViveSR_Layers.DefaultLayerName, ViveSR_Layers.TransparentFXLayerName,
                ViveSR_Layers.IgnoreRaycastLayerName, ViveSR_Layers.WaterLayerName, ViveSR_Layers.UILayerName, layer_name);
            int layer = LayerMask.NameToLayer(layer_name);
            ViveSR_DualCameraRig prefabRig = PrefabUtility.GetPrefabParent(ViveSR_DualCameraRig.Instance) as ViveSR_DualCameraRig;

            Camera cam = camera_index == DualCameraIndex.LEFT ?
                prefabRig.DualCameraLeft :
                prefabRig.DualCameraRight;
            ViveSR_TrackedCamera trackedCam = camera_index == DualCameraIndex.LEFT ?
                prefabRig.TrackedCameraLeft :
                prefabRig.TrackedCameraRight;

            cam.cullingMask = mask;
            trackedCam.gameObject.layer = layer;
            trackedCam.Anchor.gameObject.layer = layer;
            trackedCam.DepthImageOccluder.gameObject.layer = layer;
            trackedCam.ImagePlane.gameObject.layer = layer;

#if UNITY_2019_1_OR_NEWER
            PrefabUtility.RevertPrefabInstance(ViveSR_DualCameraRig.Instance.gameObject, InteractionMode.AutomatedAction);
#else
            PrefabUtility.RevertPrefabInstance(ViveSR_DualCameraRig.Instance.gameObject);
#endif
        }

        private void UpdateRenderCameraLayers()
        {
            ViveSR_DualCameraRig prefabRig = PrefabUtility.GetPrefabParent(ViveSR_DualCameraRig.Instance) as ViveSR_DualCameraRig;

            prefabRig.RenderCameraLeft.cullingMask = LayerMask.GetMask(ViveSR_Layers.RenderPlaneLeftLayerName);
            prefabRig.RenderCameraRight.cullingMask = LayerMask.GetMask(ViveSR_Layers.RenderPlaneRightLayerName);
            prefabRig.TrackedCameraLeft.RenderPlane.gameObject.layer = LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneLeftLayerName);
            prefabRig.TrackedCameraRight.RenderPlane.gameObject.layer = LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneRightLayerName);
            prefabRig.RenderCameraFrameLeft.gameObject.layer = LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneLeftLayerName);
            prefabRig.RenderCameraFrameRight.gameObject.layer = LayerMask.NameToLayer(ViveSR_Layers.RenderPlaneRightLayerName);

#if UNITY_2019_1_OR_NEWER
            PrefabUtility.RevertPrefabInstance(ViveSR_DualCameraRig.Instance.gameObject, InteractionMode.AutomatedAction);
#else
            PrefabUtility.RevertPrefabInstance(ViveSR_DualCameraRig.Instance.gameObject);
#endif
        }

        private bool ModifyLayer(int index, string name)
        {
            if (index < 0 || index > 31) return false;
            if (LayerMask.NameToLayer(name) == index) return true;
            SerializedObject tag_manager = new SerializedObject(AssetDatabase.LoadAllAssetsAtPath("ProjectSettings/TagManager.asset")[0]);
            SerializedProperty layers = tag_manager.FindProperty("layers");
            if (layers == null) return false;
            if (!layers.isArray) return false;
            layers.GetArrayElementAtIndex(index).stringValue = name;
            tag_manager.ApplyModifiedProperties();
            return true;
        }
    }

    public class Page_Finish : ViveSR_Settings.Page
    {
        public override string Name { get { return "Finish"; } }

        private bool click = false;
        public override bool IsNeedShow()
        {
            return !click;
        }

        public override void RenderGUI()
        {
            GUILayout.Label("Let's start SRWorks!");
            GUILayout.FlexibleSpace();
            if (GUILayout.Button("Close")) click = true;
        }
    }

    public class Page_QualitySetting : ViveSR_Settings.Page
    {
        public override string Name { get { return "Quality Setting"; } }

        private const short X_FROM = 210;
        private const short X_TO = 265;
        private const short W_ITEM = 50;
        private const short H_ITEM = 20;
        public bool AutoQualitySetting
        {
            get { return EditorPrefs.GetBool("ViveSR_AutoQualitySetting", true); }
            set { EditorPrefs.SetBool("ViveSR_AutoQualitySetting", value); }
        }
        ShadowQuality recommended_Shadows = ShadowQuality.All;
        ShadowResolution recommended_ShadowResolution = ShadowResolution.High;
        int recommended_ShadowDistance = 20;
        int recommended_ShadowCascades = 2;

        bool IsOkShadowQuality { get { return QualitySettings.shadows == recommended_Shadows; } }
        bool IsOkShadowResolution { get { return (int)QualitySettings.shadowResolution >= (int)recommended_ShadowResolution; } }
        bool IsOkShadowDistance { get { return QualitySettings.shadowDistance <= recommended_ShadowDistance; } }
        bool IsOkShadowCascades { get { return QualitySettings.shadowCascades == recommended_ShadowCascades; } }

        public override bool IsNeedShow()
        {
            AutoQualitySetting = true;
            bool show = AutoQualitySetting && !(IsOkShadowQuality && IsOkShadowResolution &&
                                                IsOkShadowDistance && IsOkShadowCascades);
            return show;
        }

        public override void RenderGUI()
        {
            GUILayout.BeginHorizontal();
            GUILayout.Label("Item", EditorStyles.boldLabel);
            Rect rect = GUILayoutUtility.GetLastRect();
            GUI.Label(new Rect(rect.x + X_FROM, rect.y, W_ITEM, H_ITEM), "From", EditorStyles.boldLabel);
            GUI.Label(new Rect(rect.x + X_TO, rect.y, W_ITEM, H_ITEM), "To", EditorStyles.boldLabel);
            GUILayout.EndHorizontal();

            MyLabel("QualitySettings.Shadows ", QualitySettings.shadows.ToString(), recommended_Shadows.ToString(), IsOkShadowQuality);
            MyLabel("QualitySettings.ShadowResolution ", QualitySettings.shadowResolution.ToString(), recommended_ShadowResolution.ToString(), IsOkShadowResolution);
            MyLabel("QualitySettings.ShadowDistance ", QualitySettings.shadowDistance.ToString(), recommended_ShadowDistance.ToString(), IsOkShadowDistance);
            MyLabel("QualitySettings.ShadowCascades ", QualitySettings.shadowCascades.ToString(), recommended_ShadowCascades.ToString(), IsOkShadowCascades);

            GUILayout.FlexibleSpace();
            GUILayout.BeginHorizontal();
            if (GUILayout.Button("Ignore")) AutoQualitySetting = false;
            if (GUILayout.Button("Accept")) ChangeQualitySetting();
            GUILayout.EndHorizontal();
        }

        private void MyLabel(string label, string current, string target, bool isOK)
        {
            if (isOK) return;
            GUILayout.BeginHorizontal();
            GUILayout.Label(label);
            Rect rect = GUILayoutUtility.GetLastRect();
            GUI.Label(new Rect(rect.x + X_FROM, rect.y, W_ITEM, H_ITEM), current);
            GUI.Label(new Rect(rect.x + X_TO, rect.y, W_ITEM, H_ITEM), target);
            GUILayout.EndHorizontal();
        }

        private void ChangeQualitySetting()
        {
            QualitySettings.shadows = recommended_Shadows;
            if ((int)QualitySettings.shadowResolution < (int)recommended_ShadowResolution) QualitySettings.shadowResolution = recommended_ShadowResolution;
            if (QualitySettings.shadowDistance > recommended_ShadowDistance) QualitySettings.shadowDistance = recommended_ShadowDistance;
            QualitySettings.shadowCascades = recommended_ShadowCascades;
        }
    }
}
#endif