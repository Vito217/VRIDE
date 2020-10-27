#define TextureScaler
#define ByDepth
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Runtime.InteropServices;

namespace Vive.Plugin.SR
{
    public class ViveSR_AISceneRenderer : MonoBehaviour
    {
        private SegmentWay SegmentMethod;
        private SegmentWay LastSegmentMethod;
        public DualCameraIndex CameraIndex;
        public int ImageWidth,ImageHeight;
        public int LabelWidth, LabelHeight;
        public int DepthWidth, DepthHeight, DepthDataSize;
        public double FocalLength, DepthFocalLength;
        public Texture2D SegmentTexture;
        private byte[] LabelMap;
        private Texture2D LabelTexture32; //for shader
        Color32[] LabelRGBA;
        public static IntPtr WarpDepthMap;
        public Texture2D DepthTexture;
        private float MaxDistance = 200.0f;
        private float MinDistance = 40.0f;
        private Color BackgroundColor;
        private float[] HSVMaxMin = new float[6];
        private float[] HSVThreshold = new float[3];
        public GameObject GameObjectParent;
        private Matrix4x4 PoseUndistorted = new Matrix4x4();
        private float[] RawUndistortedPose = new float[16];
        Renderer Rnd;
        private MeshRenderer MeshRnd;
        private Material DefaultMat;
        ViveSR_DualCameraImagePlane ImagePlane;
        public ViveSR_DualCameraImageRenderer ImageRenderer;
        private bool InitialComplete = false;

        public void SetSegmentMethod(SegmentWay method)
        {
            SegmentMethod = method;
            SetSegmentSetting(SegmentMethod);
        }
        public SegmentWay GetSegmentMethod()
        {
            return SegmentMethod;
        }
        public void SetMaxMinDistance(float max,float min)
        {
            MaxDistance = max;
            MinDistance = min;
        }
        public void GetMaxMinDistance(out float max, out float min)
        {
            max = MaxDistance;
            min = MinDistance;
        }
        public void SetBackgroundColor(Color c)
        {
            float h, s, v;
            Color.RGBToHSV(c, out h, out s, out v);
            HSVMaxMin[0] = h + HSVThreshold[0];
            HSVMaxMin[1] = h - HSVThreshold[0];
            HSVMaxMin[2] = 1.0f;
            HSVMaxMin[3] = s - HSVThreshold[1];
            HSVMaxMin[4] = 1.0f;
            HSVMaxMin[5] = v - HSVThreshold[2];
            BackgroundColor = c;
        }
        public void GetBackgroundColor(out Color c)
        {
            c = BackgroundColor;
        }
        // Use this for initialization
        public void initial (SegmentWay method) {
            if (!ViveSR.Instance.InitializeAISceneModule)
                return;
            SegmentMethod = method;
            LastSegmentMethod = SegmentMethod;
            SRWorkModule_API.GetDepthParameterDouble((int)DepthParam.FOCULENS, ref DepthFocalLength);
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                ImageWidth = PassThrough.SRWork_PassThrough.Undistorted4KImageWidth;
                ImageHeight = PassThrough.SRWork_PassThrough.Undistorted4KImageHeight;
                FocalLength = ViveSR_DualCameraImageCapture.FocalLengthLeft;
                LabelWidth = AIScene.SRWork_AI_Scene.LabelImageWidth;
                LabelHeight = AIScene.SRWork_AI_Scene.LabelImageHeight;
                DepthWidth = ViveSR_DualCameraImageCapture.DepthImageWidth;
                DepthHeight = ViveSR_DualCameraImageCapture.DepthImageHeight;
                DepthDataSize = ViveSR_DualCameraImageCapture.DepthDataSize;
                HSVMaxMin[0] = 0.5f;
                HSVMaxMin[1] = 0.3f;
                HSVMaxMin[2] = 1.0f;
                HSVMaxMin[3] = 0.3783f;
                HSVMaxMin[4] = 1.0f;
                HSVMaxMin[5] = 0.1995f;
                HSVThreshold[0] = 0.07f;
                HSVThreshold[1] = 0.1f;
                HSVThreshold[2] = 0.1f;
            }
            else
            {
                ImageWidth = PassThrough.SRWork_PassThrough.UndistortedImageWidth;
                ImageHeight = PassThrough.SRWork_PassThrough.UndistortedImageHeight;
                FocalLength = ViveSR_DualCameraImageCapture.FocalLengthLeft;
                LabelWidth = AIScene.SRWork_AI_Scene.LabelImageWidth;
                LabelHeight = AIScene.SRWork_AI_Scene.LabelImageHeight;
                DepthWidth = ViveSR_DualCameraImageCapture.DepthImageWidth;
                DepthHeight = ViveSR_DualCameraImageCapture.DepthImageHeight;
                DepthDataSize = ViveSR_DualCameraImageCapture.DepthDataSize;
                HSVMaxMin[0] = 0.5f;
                HSVMaxMin[1] = 0.1222f;
                HSVMaxMin[2] = 1.0f;
                HSVMaxMin[3] = 0.8305f;
                HSVMaxMin[4] = 1.0f;
                HSVMaxMin[5] = 0.0784f;
                HSVThreshold[0] = 0.2f;
                HSVThreshold[1] = 0.1f;
                HSVThreshold[2] = 0.2f;
                int deviceType = (int)VRDevice.None;
                SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.DEVICE_SYSTEM_TYPE, ref deviceType);
                if (VRDevice.VIVE_PRO == (VRDevice)deviceType)
                {
                    HSVMaxMin[3] = 0.3965f;
                }
            }
            BackgroundColor = Color.HSVToRGB((HSVMaxMin[0] + HSVMaxMin[1]) * 0.5f, HSVMaxMin[3] + HSVThreshold[1], HSVMaxMin[5] + HSVThreshold[2]);
            SegmentTexture = new Texture2D(ImageWidth, ImageHeight, TextureFormat.RGBA32, false);
            LabelMap = new byte[LabelWidth * LabelHeight * 1];
            LabelRGBA = new Color32[LabelMap.Length];
            LabelTexture32 = new Texture2D(LabelWidth, LabelHeight, TextureFormat.RGBA32, false);
            DepthTexture = new Texture2D(DepthWidth, DepthHeight, TextureFormat.RFloat, false);
            WarpDepthMap = Marshal.AllocCoTaskMem(sizeof(float) * DepthWidth * DepthHeight * 1);
            ImagePlane = GetComponent<ViveSR_DualCameraImagePlane>();
            ImagePlane.Initial(ImageWidth, ImageHeight, 0.5f * ImageWidth, 0.5f * ImageHeight, FocalLength, false);
            SetSegmentSetting(SegmentMethod);
            Rnd = GetComponent<Renderer>();
            Rnd.material.SetInt("_SegmentWay", (int)SegmentMethod);
            Rnd.material.mainTexture = SegmentTexture;
            MeshRnd = GetComponent<MeshRenderer>();
            if (MeshRnd) DefaultMat = MeshRnd.sharedMaterial;
            DefaultMat.shader = Shader.Find("ViveSR/Segmentation");
            InitialComplete = true;
        }

        // Update is called once per frame
        void Update ()
        {
            if (!InitialComplete)
                return;
            if (FrameworkStatus.WORKING!=ViveSR.FrameworkStatus)
                return;

            ChangeSegmentMethod(SegmentMethod);
            if (PassThrough.SRWork_PassThrough.Image4kReady)
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_left, RawUndistortedPose, 0, RawUndistortedPose.Length);
            else
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_data_.pose_left, RawUndistortedPose, 0, RawUndistortedPose.Length);

            ParseUndistortedPtrData();
            SetCameraPoses();
            if(null == ImageRenderer.UndistortedLeftCameraImageMaterials[0].mainTexture || null == SegmentTexture)
                return;
            Graphics.CopyTexture(ImageRenderer.UndistortedLeftCameraImageMaterials[0].mainTexture, SegmentTexture);

            switch (SegmentMethod)
            {
                case SegmentWay.AI_SCENE:
                {
                    AISceneMethod();
                    break;
                }
                case SegmentWay.DEPTH:
                {
                    DepthMethod();
                    break;
                }
                case  SegmentWay.BACKGROUND_COLOR:
                {
                    BackgroundColorMethod();
                    break;
                }
            }
	    }
        private void SetCameraPoses()
        {
            var position = ViveSR_DualCameraImageCapture.Position(PoseUndistorted);
            var rotation = ViveSR_DualCameraImageCapture.Rotation(PoseUndistorted);
            // Set the GameObjectParent poses.
            GameObjectParent.transform.localPosition = position;
            GameObjectParent.transform.localRotation = rotation;
        }
        private void ParseUndistortedPtrData()
        {
            for (int i = 0; i < 4; i++)
            {
                PoseUndistorted.SetColumn(i, new Vector4(RawUndistortedPose[i * 4 + 0], RawUndistortedPose[i * 4 + 1],
                                                             RawUndistortedPose[i * 4 + 2], RawUndistortedPose[i * 4 + 3]));
            }
        }
        private void ChangeSegmentMethod(SegmentWay NowMethod)
        {
            if(LastSegmentMethod == NowMethod)
                return;
            LastSegmentMethod = NowMethod;
            Rnd.material.SetInt("_SegmentWay", (int)NowMethod);
        }
        private void SetSegmentSetting(SegmentWay NowMethod)
        {
            switch (NowMethod)
            {
                case SegmentWay.AI_SCENE:
                    ViveSR_DualCameraImageRenderer.UpdateDepthMaterial = false;
                    break;
                case SegmentWay.DEPTH:
                    ViveSR_DualCameraImageRenderer.UpdateDepthMaterial = true;
                    break;
                case SegmentWay.BACKGROUND_COLOR:
                    ViveSR_DualCameraImageRenderer.UpdateDepthMaterial = false;
                    break;
                default:
                    break;
            }
        }
        private void AISceneMethod()
        {
            Marshal.Copy(
                AIScene.SRWork_AI_Scene.ai_scene_data_.label_left,
                LabelMap,
                0,
                LabelMap.Length);
            for (int i = 0; i < LabelHeight; i++)
            {
                for (int j = 0; j < LabelWidth; j++)
                {
                    if (LabelMap[i * LabelWidth + j] != (byte)HumanLabels.IS_HUMAN) //NON HUMAN
                        LabelMap[i * LabelWidth + j] = 255;
                    else
                        LabelMap[i * LabelWidth + j] = 0; // IS HUMAN
                    byte v = LabelMap[i * LabelWidth + j];
                    LabelRGBA[i * LabelWidth + j] = new Color32(v, v, v, 255);
                }
            }
            LabelTexture32.SetPixels32(LabelRGBA);
            LabelTexture32.Apply();
            Rnd.material.SetTexture("_MaskTex", LabelTexture32);
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                Rnd.material.SetInt("_Image4kReady", 1);
                Rnd.material.SetFloat("_Scale", (float)ImageWidth / (float)LabelWidth);
            }
            else
            {
                Rnd.material.SetInt("_Image4kReady", 0);
            }
        }
        private void DepthMethod()
        {
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                Rnd.material.SetInt("_Image4kReady", 1);
                SRWorkModule_API.DepthWarp_Warp(
                Depth.SRWork_Depth.depth_data_.depth_map,
                WarpDepthMap, Depth.SRWork_Depth.depth_data_.pose,
                PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_left,
                (float)DepthFocalLength);
                DepthTexture.LoadRawTextureData(WarpDepthMap, DepthWidth * DepthHeight * DepthDataSize);
                DepthTexture.Apply();
                Rnd.material.SetTexture("_MaskTex", DepthTexture);
                Rnd.material.SetFloat("_Scale", (float)(FocalLength / DepthFocalLength));
            }
            else
            {
                Rnd.material.SetInt("_Image4kReady", 0);
                DepthTexture.LoadRawTextureData(Depth.SRWork_Depth.depth_data_.depth_map, DepthWidth * DepthHeight * DepthDataSize);
                DepthTexture.Apply();
                Rnd.material.SetTexture("_MaskTex", DepthTexture);
            }
            Rnd.material.SetFloat("_MinDistance", MinDistance);
            Rnd.material.SetFloat("_MaxDistance", MaxDistance);
        }

        private void BackgroundColorMethod()
        {
            Rnd.material.SetFloat("_HueMax", HSVMaxMin[0]);
            Rnd.material.SetFloat("_HueMin", HSVMaxMin[1]);
            Rnd.material.SetFloat("_SaturationMax", HSVMaxMin[2]);
            Rnd.material.SetFloat("_SaturationMin", HSVMaxMin[3]);
            Rnd.material.SetFloat("_ValueMax", HSVMaxMin[4]);
            Rnd.material.SetFloat("_ValueMin", HSVMaxMin[5]);
        }
    }
}
