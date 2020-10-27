using System.Collections;
using System.Collections.Generic;
using UnityEngine;
namespace Vive.Plugin.SR
{
    public enum HumanLabels
    {
        IS_HUMAN = 1,
        NON_HUMAN = 2
    };
    public enum SegmentWay
    {
        AI_SCENE,
        DEPTH,
        BACKGROUND_COLOR
    };
    public class ViveSR_AIScene : MonoBehaviour {

        private static ViveSR_AIScene Mgr = null;
        public static ViveSR_AIScene Instance
        {
            get
            {
                if (Mgr == null)
                {
                    Mgr = FindObjectOfType<ViveSR_AIScene>();
                }
                return Mgr;
            }
        }
        private SegmentWay SegmentMethod;
        public GameObject[] AIObject = new GameObject[2];
        public ViveSR_AISceneRenderer AISegmentPlaneLeft;
        public static bool IsHumanCutProcessing { get; private set; }
        public static bool IsAISceneProcessing { get; private set; }
        private DualCameraDisplayMode PreviousMode;
        private bool PreviousDepthMode;
        public static bool Initial { get; set; }
        // Use this for initialization
        void Start ()
        {
            Initial = false;
            IsAISceneProcessing = false;
            IsHumanCutProcessing = false;
        }
        public void StartHumanCut()
        {
            PreviousMode = ViveSR_DualCameraRig.Instance.Mode;
            PreviousDepthMode = ViveSR_DualCameraImageRenderer.UpdateDepthMaterial;
            ViveSR_DualCameraRig.Instance.SetMode(DualCameraDisplayMode.VIRTUAL);
            for (int i = 0; i < 2; i++)
                AIObject[i].SetActive(true);
        }
        public void StopHumanCut()
        {
            ViveSR_DualCameraRig.Instance.SetMode(PreviousMode);
            ViveSR_DualCameraImageRenderer.UpdateDepthMaterial= PreviousDepthMode;
            for (int i = 0; i < 2; i++)
                AIObject[i].SetActive(false);
        }
        public void EnableHumanCutProcess(bool active)
        {
            if(active)
            {
                IsHumanCutProcessing = true;
                StartHumanCut();
            }
            else
            {
                IsHumanCutProcessing = false;
                StopHumanCut();
            }
        }
        public int EnableAISceneProcess(bool active)
        {
            int result = (int)Error.FAILED;
            if (active)
            {
                if (PassThrough.SRWork_PassThrough.Image4kReady)
                    result = SRWorkModule_API.LinkModule((int)ModuleType.PASSTHROUGH4K, (int)ModuleType.AI_Scene);
                else
                    result = SRWorkModule_API.LinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.AI_Scene);
                if (result == (int)Error.WORK)
                {
                    IsAISceneProcessing = true;
                }
                else
                {
                    IsAISceneProcessing = false;
                }
            }
            else
            {
                if (PassThrough.SRWork_PassThrough.Image4kReady)
                    result = SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH4K, (int)ModuleType.AI_Scene);
                else
                    result = SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.AI_Scene);
                if (result == (int)Error.WORK)
                {
                    IsAISceneProcessing = false;
                }
            }
            return result;
        }
    }
}