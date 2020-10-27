//========= Copyright 2017, HTC Corporation. All rights reserved. ===========

using UnityEngine;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Vive.Plugin.SR
{
    /// <summary>
    /// This is the wrapper for converting datas to fit unity format.
    /// </summary>
    public class ViveSR_DualCameraImageCapture
    {
        [Obsolete("use ViveSR_Framework.CallbackBasic instead.")]
        public delegate void CallbackWithPose(IntPtr ptr1, IntPtr ptr2, int int1, int int2, IntPtr pose1);
        [Obsolete("use ViveSR_Framework.CallbackBasic instead.")]
        public delegate void CallbackWith2Pose(IntPtr ptr1, IntPtr ptr2, int int1, int int2, IntPtr pose1, IntPtr pose2);

        private static int[] RawDistortedFrameIndex = new int[1];
        private static int[] RawUndistortedFrameIndex = new int[1];
        private static int[] RawDepthFrameIndex = new int[1];

        private static int[] RawDistortedTimeIndex = new int[1];
        private static int[] RawUndistortedTimeIndex = new int[1];
        private static int[] RawDepthTimeIndex = new int[1];

        private static float[] RawDistortedPoseLeft = new float[16];
        private static float[] RawDistortedPoseRight = new float[16];
        private static float[] RawUndistortedPoseLeft = new float[16];
        private static float[] RawUndistortedPoseRight = new float[16];
        private static float[] RawDepthPose = new float[16];
        public static Matrix4x4 DistortedPoseLeft, DistortedPoseRight;
        public static Matrix4x4 UndistortedPoseLeft, UndistortedPoseRight;
        public static Matrix4x4 DepthPose;

        private static bool InitialUndistortedPtrSize;
        private static bool InitialDepthPtrSize;
        private static bool SetTextureFromHandleReady;

        private static int RingBufferSize;

        private static Texture2D TextureDistortedLeft;
        private static Texture2D TextureDistortedRight;
        private static Texture2D TextureUndistortedLeft;
        private static Texture2D TextureUndistortedRight;
        private static Texture2D TextureUndistortedDisplayLeft;
        private static Texture2D TextureUndistortedDisplayRight;
        private static Texture2D TextureDepth;

        private class UndistortedTextureData
        {
            public Texture2D Left;
            public Texture2D Right;
            public bool Initialized = false;
        }

        private static List<UndistortedTextureData> UndistortedTextureDataBuffer = new List<UndistortedTextureData>();

        public static double DistortedCxLeft;
        public static double DistortedCyLeft;
        public static double DistortedCxRight;
        public static double DistortedCyRight;
        public static double UndistortedCxLeft;
        public static double UndistortedCyLeft;
        public static double UndistortedCxRight;
        public static double UndistortedCyRight;
        public static double FocalLengthLeft { get; private set; }
        public static double FocalLengthRight { get; private set; }
        public static double Baseline;
        public static int DistortedImageWidth = 0, DistortedImageHeight = 0, DistortedImageChannel = 0;
        public static int UndistortedImageWidth = 0, UndistortedImageHeight = 0, UndistortedImageChannel = 0;
        public static int DepthImageWidth = 0, DepthImageHeight = 0, DepthImageChannel = 0, DepthDataSize = 4;
        public static float[] OffsetHeadToCamera = new float[6];

        public static float[] UndistortionMapLeft;
        public static float[] UndistortionMapRight;

        public static int DistortedFrameIndex { get { return RawDistortedFrameIndex[0]; } }
        public static int DistortedTimeIndex { get { return RawDistortedTimeIndex[0]; } }
        public static int UndistortedFrameIndex { get { return RawUndistortedFrameIndex[0]; } }
        public static int UndistortedTimeIndex { get { return RawUndistortedTimeIndex[0]; } }
        public static int DepthFrameIndex { get { return RawDepthFrameIndex[0]; } }
        public static int DepthTimeIndex { get { return RawDepthTimeIndex[0]; } }

        public static bool DistortTextureIsNative = false;
        public static bool UndistortTextureIsNative = false;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable DistortedCx_L has changed to DistortedCxLeft.
        * @warning The variable will remove in the future.
        */
        public static double DistortedCx_L
        {
            get { return DistortedCxLeft; }
            set { DistortedCxLeft = value; }
        }
        /**
        * The variable DistortedCy_L has changed to DistortedCyLeft.
        * @warning The variable will remove in the future.
        */
        public static double DistortedCy_L
        {
            get { return DistortedCyLeft; }
            set { DistortedCyLeft = value; }
        }
        /**
        * The variable DistortedCx_R has changed to DistortedCxRight.
        * @warning The variable will remove in the future.
        */
        public static double DistortedCx_R
        {
            get { return DistortedCxRight; }
            set { DistortedCxRight = value; }
        }
        /**
        * The variable DistortedCy_R has changed to DistortedCyRight.
        * @warning The variable will remove in the future.
        */
        public static double DistortedCy_R
        {
            get { return DistortedCyRight; }
            set { DistortedCyRight = value; }
        }
        /**
        * The variable UndistortedCx_L has changed to UndistortedCxLeft.
        * @warning The variable will remove in the future.
        */
        public static double UndistortedCx_L
        {
            get { return UndistortedCxLeft; }
            set { UndistortedCxLeft = value; }
        }
        /**
        * The variable UndistortedCy_L has changed to UndistortedCyLeft.
        * @warning The variable will remove in the future.
        */
        public static double UndistortedCy_L
        {
            get { return UndistortedCyLeft; }
            set { UndistortedCyLeft = value; }
        }
        /**
        * The variable UndistortedCx_R has changed to UndistortedCxRight.
        * @warning The variable will remove in the future.
        */
        public static double UndistortedCx_R
        {
            get { return UndistortedCxRight; }
            set { UndistortedCxRight = value; }
        }
        /**
        * The variable UndistortedCy_R has changed to UndistortedCyRight.
        * @warning The variable will remove in the future.
        */
        public static double UndistortedCy_R
        {
            get { return UndistortedCyRight; }
            set { UndistortedCyRight = value; }
        }
        /**
        * The variable FocalLength_L has changed to FocalLengthLeft.
        * @warning The variable will remove in the future.
        */
        public static double FocalLength_L
        {
            get { return FocalLengthLeft; }
            private set { }
        }
        /**
        * The variable FocalLength_R has changed to FocalLengthLeft.
        * @warning The variable will remove in the future.
        */
        public static double FocalLength_R
        {
            get { return FocalLengthLeft; }
            private set { }
        }
        /**
        * The variable UndistortionMap_L has changed to UndistortionMapLeft.
        * @warning The variable will remove in the future.
        */
        public static float[] UndistortionMap_L
        {
            get { return UndistortionMapLeft; }
            set { UndistortionMapLeft = value; }
        }
        /**
        * The variable UndistortionMap_R has changed to UndistortionMapRight.
        * @warning The variable will remove in the future.
        */
        public static float[] UndistortionMap_R
        {
            get { return UndistortionMapRight; }
            set { UndistortionMapRight = value; }
        }
        /**
        * The variable DepthProcessing has changed to IsDepthProcessing.
        * @warning The variable will remove in the future.
        */
        public static bool DepthProcessing
        {
            get { return IsDepthProcessing; }
            private set { }
        }
        /**
        * The variable DepthRefinement has changed to IsDepthRefinementEnabled.
        * @warning The variable will remove in the future.
        */
        public static bool DepthRefinement
        {
            get { return IsDepthRefinementEnabled; }
            set { IsDepthRefinementEnabled = value; }
        }
        /**
        * The variable DepthEdgeEnhance has changed to IsDepthEdgeEnhanceEnabled.
        * @warning The variable will remove in the future.
        */
        public static bool DepthEdgeEnhance
        {
            get { return IsDepthEdgeEnhanceEnabled; }
            set { IsDepthEdgeEnhanceEnabled = value; }
        }
        /**
        * The function ChangeDepthCase has changed to SetDepthCase.
        * @warning The function will remove in the future.
        */
        public static int ChangeDepthCase(DepthCase depthCase)
        {
            return SetDepthCase(depthCase);
        }
        /**
        * The structure CameraQualityInfo has changed to CameraSettingInfo.
        * @warning The structure will remove in the future.
        */
        public struct CameraQualityInfo
        {
            public Int32 Status;
            public Int32 DefaultValue;
            public Int32 Min;
            public Int32 Max;
            public Int32 Step;
            public Int32 DefaultMode;
            public Int32 Value;
            public Int32 Mode;  // AUTO = 1, MANUAL = 2
        };
        /**
        * The function GetCameraQualityInfo has changed to GetCameraSettingInfo.
        * @warning The function will remove in the future.
        */
        public static int GetCameraQualityInfo(CameraQuality item, ref CameraQualityInfo paramInfo)
        {
            int result = (int)Error.FAILED;
            result = SRWorkModule_API.GetCameraStatus((int)item, ref paramInfo.Status);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraDefaultValue((int)item, ref paramInfo.DefaultValue);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMin((int)item, ref paramInfo.Min);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMax((int)item, ref paramInfo.Max);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraStep((int)item, ref paramInfo.Step);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraDefaultMode((int)item, ref paramInfo.DefaultMode);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraValue((int)item, ref paramInfo.Value);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMode((int)item, ref paramInfo.Mode);
            if (result != (int)Error.WORK) return result;

            return result;
        }
        /**
        * The function SetCameraQualityInfo has changed to SetCameraSettingInfo.
        * @warning The function will remove in the future.
        */
        public static int SetCameraQualityInfo(CameraQuality item, CameraQualityInfo paramInfo)
        {
            int result = (int)Error.FAILED;
            result = SRWorkModule_API.SetCameraStatus((int)item, paramInfo.Status);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraDefaultValue((int)item, paramInfo.DefaultValue);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMin((int)item, paramInfo.Min);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMax((int)item, paramInfo.Max);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraStep((int)item, paramInfo.Step);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraDefaultMode((int)item, paramInfo.DefaultMode);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraValue((int)item, paramInfo.Value);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMode((int)item, paramInfo.Mode);
            if (result != (int)Error.WORK) return result;
            return result;
        }
        /**
        * The enum CameraQuality has changed to CameraSettingo.
        * @warning The enum will remove in the future.
        */
        public enum CameraQuality
        {
            BRIGHTNESS = 100,
            CONTRAST = 101,
            HUE = 102,
            SATURATION = 103,
            SHARPNESS = 104,
            GAMMA = 105,
            //COLOR_ENABLE          = 106,
            WHITE_BALANCE = 107,
            BACKLIGHT_COMPENSATION = 108,
            GAIN = 109,
            //PAN                   = 110,
            //TILT                  = 111,
            //ROLL                  = 112,
            //ZOOM                  = 113,
            //EXPOSURE              = 114,
            //IRIS                  = 115,
            //FOCUS                 = 116,
        };
        #endregion

        /// <summary>
        /// Initialize the image capturing tool.
        /// </summary>
        /// <returns></returns>
        public static int Initial()
        {
            GetParameters();
            SRWorkModule_API.CreateCopyD3D11TextureBuffer();
            SetTextureFromHandleReady = false;

            RingBufferSize = SRWorkModule_API.GetRingBufferSize();
            for (int i = 0; i < RingBufferSize; i++)
                UndistortedTextureDataBuffer.Add(new UndistortedTextureData());

            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                TextureDistortedLeft = new Texture2D(DistortedImageWidth, DistortedImageHeight, TextureFormat.RGBA32, false);
                TextureDistortedRight = new Texture2D(DistortedImageWidth, DistortedImageHeight, TextureFormat.RGBA32, false);
                TextureUndistortedLeft = new Texture2D(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false);
                TextureUndistortedRight = new Texture2D(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false);
            }
            else
            {
                TextureDistortedLeft = new Texture2D(DistortedImageWidth, DistortedImageHeight, TextureFormat.RGBA32, false);
                TextureDistortedRight = new Texture2D(DistortedImageWidth, DistortedImageHeight, TextureFormat.RGBA32, false);
                TextureUndistortedLeft = new Texture2D(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false);
                TextureUndistortedRight = new Texture2D(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false);
            }

            TextureDepth = new Texture2D(DepthImageWidth, DepthImageHeight, TextureFormat.RFloat, false);
            return (int)Error.WORK;
        }

        public static void Release()
        {
            Texture2D.Destroy(TextureDistortedLeft);
            Texture2D.Destroy(TextureDistortedRight);
            Texture2D.Destroy(TextureUndistortedLeft);
            Texture2D.Destroy(TextureUndistortedRight);
            Texture2D.Destroy(TextureDepth);

            for(int i = 0; i < UndistortedTextureDataBuffer.Count; i++)
            {
                if (UndistortedTextureDataBuffer[i].Initialized)
                {
                    Texture2D.Destroy(UndistortedTextureDataBuffer[i].Left);
                    Texture2D.Destroy(UndistortedTextureDataBuffer[i].Right);
                    UndistortedTextureDataBuffer[i].Initialized = false;
                }
            }

            UndistortedTextureDataBuffer.Clear();
            SetTextureFromHandleReady = false;
            SRWorkModule_API.ReleaseCopyD3D11TextureBuffer();

            TextureDistortedLeft = null;
            TextureDistortedRight = null;
            TextureUndistortedLeft = null;
            TextureUndistortedRight = null;
            TextureDepth = null;
        }

        private static void GetParameters()
        {
            double[] cameraparams = new double[(int)CAMERA_Param.CAMERA_PARAMS_MAX];
            if (PassThrough.SRWork_PassThrough.Image4kReady)
                SRWorkModule_API.Get4KCameraParams(cameraparams);
            else
                SRWorkModule_API.GetCameraParams(cameraparams);

            DistortedCxLeft = cameraparams[0];
            DistortedCyLeft = cameraparams[2];
            DistortedCxRight = cameraparams[1];
            DistortedCyRight = cameraparams[3];
            FocalLengthLeft = cameraparams[4];
            FocalLengthRight = cameraparams[5];

            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_DISTORTED_4K_WIDTH, ref DistortedImageWidth);
                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_DISTORTED_4K_HEIGHT, ref DistortedImageHeight);
                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_DISTORTED_4K_CHANNEL, ref DistortedImageChannel);

                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_UNDISTORTED_4K_WIDTH, ref UndistortedImageWidth);
                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_UNDISTORTED_4K_HEIGHT, ref UndistortedImageHeight);
                SRWorkModule_API.GetPassThroug4KParameterInt((int)PassThrough4KParam.OUTPUT_UNDISTORTED_4K_CHANNEL, ref UndistortedImageChannel);
            }
            else
            {
                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_DISTORTED_WIDTH, ref DistortedImageWidth);
                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_DISTORTED_HEIGHT, ref DistortedImageHeight);
                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_DISTORTED_CHANNEL, ref DistortedImageChannel);

                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_UNDISTORTED_WIDTH, ref UndistortedImageWidth);
                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_UNDISTORTED_HEIGHT, ref UndistortedImageHeight);
                SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.OUTPUT_UNDISTORTED_CHANNEL, ref UndistortedImageChannel);
            }

            if (ViveSR.Instance.InitializeDepthModule)
            {
                SRWorkModule_API.GetDepthParameterInt((int)DepthParam.OUTPUT_WIDTH, ref DepthImageWidth);
                SRWorkModule_API.GetDepthParameterInt((int)DepthParam.OUTPUT_HEIGHT, ref DepthImageHeight);
                SRWorkModule_API.GetDepthParameterInt((int)DepthParam.OUTPUT_CHAANEL_1, ref DepthImageChannel);
                SRWorkModule_API.GetDepthParameterDouble((int)DepthParam.BASELINE, ref Baseline);
            }

            int undistortionMapSize = 0;
            SRWorkModule_API.GetPassThrougParameterInt((int)PassThroughParam.UNDISTORTION_MAP_SIZE, ref undistortionMapSize);            

            UndistortedCxLeft = cameraparams[18];
            UndistortedCyLeft = cameraparams[19];
            UndistortedCxRight = cameraparams[20];
            UndistortedCyRight = cameraparams[21];

            //Get offset head to camera from hardware design.
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_x0, ref OffsetHeadToCamera[0]);
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_y0, ref OffsetHeadToCamera[1]);
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_z0, ref OffsetHeadToCamera[2]);
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_x1, ref OffsetHeadToCamera[3]);
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_y1, ref OffsetHeadToCamera[4]);
                SRWorkModule_API.GetPassThroug4KParameterFloat((int)PassThrough4KParam.OFFSET_HEAD_TO_4K_CAMERA_z1, ref OffsetHeadToCamera[5]);
            }
            else
            {
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_x0, ref OffsetHeadToCamera[0]);
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_y0, ref OffsetHeadToCamera[1]);
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_z0, ref OffsetHeadToCamera[2]);
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_x1, ref OffsetHeadToCamera[3]);
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_y1, ref OffsetHeadToCamera[4]);
                SRWorkModule_API.GetPassThrougParameterFloat((int)PassThroughParam.OFFSET_HEAD_TO_CAMERA_z1, ref OffsetHeadToCamera[5]);
            }
        }

#region GetTexture2D
        /// <summary>
        /// Get the distorted texture, frame index, time index from current buffer.
        /// </summary>
        /// <param name="imageLeft"></param>
        /// <param name="imageRight"></param>
        /// <param name="frameIndex"></param>
        /// <param name="timeIndex"></param>
        public static void GetDistortedTexture(out Texture2D imageLeft, out Texture2D imageRight, out int frameIndex, out int timeIndex, out Matrix4x4 poseLeft, out Matrix4x4 poseRight)
        {
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                if (PassThrough.SRWork_PassThrough.pass_through_4k_data_.distorted_4k_frame_left != IntPtr.Zero && PassThrough.SRWork_PassThrough.pass_through_4k_data_.distorted_4k_frame_right != IntPtr.Zero)
                {
                    TextureDistortedLeft.LoadRawTextureData(PassThrough.SRWork_PassThrough.pass_through_4k_data_.distorted_4k_frame_left, DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);
                    TextureDistortedRight.LoadRawTextureData(PassThrough.SRWork_PassThrough.pass_through_4k_data_.distorted_4k_frame_right, DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);
                    TextureDistortedLeft.Apply();
                    TextureDistortedRight.Apply();
                }
            }
            else
            {
                if (PassThrough.SRWork_PassThrough.pass_through_data_.distorted_frame_left != IntPtr.Zero && PassThrough.SRWork_PassThrough.pass_through_data_.distorted_frame_right != IntPtr.Zero)
                {
                    TextureDistortedLeft.LoadRawTextureData(PassThrough.SRWork_PassThrough.pass_through_data_.distorted_frame_left, DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);
                    TextureDistortedRight.LoadRawTextureData(PassThrough.SRWork_PassThrough.pass_through_data_.distorted_frame_right, DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);
                    TextureDistortedLeft.Apply();
                    TextureDistortedRight.Apply();
                }
            }
            imageLeft = TextureDistortedLeft;
            imageRight = TextureDistortedRight;

            frameIndex = DistortedFrameIndex;
            timeIndex = DistortedTimeIndex;
            poseLeft = DistortedPoseLeft;
            poseRight = DistortedPoseRight;
        }

        /// <summary>
        /// Get the undistorted texture, frame index, time index from current buffer.
        /// </summary>
        /// <param name="imageLeft"></param>
        /// <param name="imageRight"></param>
        /// <param name="frameIndex"></param>
        /// <param name="timeIndex"></param>
        public static void GetUndistortedTexture(out Texture2D imageLeft, out Texture2D imageRight, out int frameIndex, out int timeIndex, out Matrix4x4 poseLeft, out Matrix4x4 poseRight)
        {
            IntPtr TextureUndistortedLeftFrame = PassThrough.SRWork_PassThrough.Image4kReady ? PassThrough.SRWork_PassThrough.pass_through_4k_data_.undistorted_4k_frame_left : PassThrough.SRWork_PassThrough.pass_through_data_.undistorted_frame_left;
            IntPtr TextureUndistortedRightFrame = PassThrough.SRWork_PassThrough.Image4kReady ? PassThrough.SRWork_PassThrough.pass_through_4k_data_.undistorted_4k_frame_right : PassThrough.SRWork_PassThrough.pass_through_data_.undistorted_frame_right;

            if (TextureUndistortedLeftFrame != IntPtr.Zero && TextureUndistortedRightFrame != IntPtr.Zero)
            {
                TextureUndistortedLeft.LoadRawTextureData(TextureUndistortedLeftFrame, UndistortedImageWidth * UndistortedImageHeight * UndistortedImageChannel);
                TextureUndistortedRight.LoadRawTextureData(TextureUndistortedRightFrame, UndistortedImageWidth * UndistortedImageHeight * UndistortedImageChannel);
                TextureUndistortedLeft.Apply();
                TextureUndistortedRight.Apply();
                TextureUndistortedDisplayLeft = TextureUndistortedLeft;
                TextureUndistortedDisplayRight = TextureUndistortedRight;
            }

            imageLeft = TextureUndistortedDisplayLeft;
            imageRight = TextureUndistortedDisplayRight;
            frameIndex = UndistortedFrameIndex;
            timeIndex = UndistortedTimeIndex;
            poseLeft = UndistortedPoseLeft;
            poseRight = UndistortedPoseRight;
        }

        /// <summary>
        /// Get the depth texture, frame index, time index from current buffer.
        /// </summary>
        /// <param name="imageDepth"></param>
        /// <param name="frameIndex"></param>
        /// <param name="timeIndex"></param>
        public static void GetDepthTexture(out Texture2D imageDepth, out int frameIndex, out int timeIndex, out Matrix4x4 pose)
        {

            if (Depth.SRWork_Depth.depth_data_.depth_map != IntPtr.Zero)
            {
                TextureDepth.LoadRawTextureData(Depth.SRWork_Depth.depth_data_.depth_map, DepthImageWidth * DepthImageHeight * DepthImageChannel * DepthDataSize);
                TextureDepth.Apply();
            }
            imageDepth = TextureDepth;
            frameIndex = DepthFrameIndex;
            timeIndex = DepthTimeIndex;
            pose = DepthPose;
        }
#endregion

#region Active
        /// <summary>
        /// Update the buffer of distorted texture, frame index and time index.
        /// If there is no new data received, do nothing.
        /// </summary>
        public static void UpdateDistortedImage()
        {
            if (PassThrough.SRWork_PassThrough.LastUpdateDataResult != (int) Error.WORK) return;
            ParseDistortedPtrData();
        }

        /// <summary>
        /// Update the buffer of undistorted texture, frame index and time index.
        /// If there is no new data received, do nothing.
        /// </summary>
        public static void UpdateUndistortedImage()
        {
            if (PassThrough.SRWork_PassThrough.LastUpdateDataResult != (int) Error.WORK) return;
            ParseUndistortedPtrData();
        }

        /// <summary>
        /// Update the buffer of depth texture, frame index and time index.
        /// If there is no new data received, do nothing.
        /// </summary>
        public static void UpdateDepthImage()
        {
            if (Depth.SRWork_Depth.LastUpdateDataResult != (int) Error.WORK) return;
            ParseDepthPtrData();
        }
#endregion
        
#region Parse the pointer
        private static void ParseDistortedPtrData()
        {
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                RawDistortedFrameIndex[0] = PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_frameSeq;
                RawDistortedTimeIndex[0] = PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_timeStp;
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_left, RawDistortedPoseLeft, 0, RawDistortedPoseLeft.Length);
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_right, RawDistortedPoseRight, 0, RawDistortedPoseRight.Length);
            }
            else
            {
                RawDistortedFrameIndex[0] = PassThrough.SRWork_PassThrough.pass_through_data_.frame_Seq;
                RawDistortedTimeIndex[0] = PassThrough.SRWork_PassThrough.pass_through_data_.time_Stp;
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_data_.pose_left, RawDistortedPoseLeft, 0, RawDistortedPoseLeft.Length);
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_data_.pose_right, RawDistortedPoseRight, 0, RawDistortedPoseRight.Length);
            }

            for (int i = 0; i < 4; i++)
            {
                DistortedPoseLeft.SetColumn(i, new Vector4(RawDistortedPoseLeft[i * 4 + 0], RawDistortedPoseLeft[i * 4 + 1],
                                                           RawDistortedPoseLeft[i * 4 + 2], RawDistortedPoseLeft[i * 4 + 3]));
                DistortedPoseRight.SetColumn(i, new Vector4(RawDistortedPoseRight[i * 4 + 0], RawDistortedPoseRight[i * 4 + 1],
                                                            RawDistortedPoseRight[i * 4 + 2], RawDistortedPoseRight[i * 4 + 3]));
            }
        }
        private static void ParseUndistortedPtrData()
        {
            // Parse the textures.
            // Prepare to receive GPU shared textures.
            // Get data according to whether the non-4k or the 4k see through is in use.
            uint ReceivedBufferIndex;
            System.IntPtr ReceivedPointerToSharedHandleLeft;
            System.IntPtr ReceivedPointerToSharedHandleRight;
            GetFrameGpuTextureRelatedData(PassThrough.SRWork_PassThrough.Image4kReady, out ReceivedBufferIndex,
                out ReceivedPointerToSharedHandleLeft, out ReceivedPointerToSharedHandleRight);

            if (SRWorkModule_API.SetShareHandle(ReceivedPointerToSharedHandleLeft, ReceivedPointerToSharedHandleRight, ReceivedBufferIndex,
                    PassThrough.SRWork_PassThrough.Image4kReady) && !SetTextureFromHandleReady)
            {
                //Use render event to create D3D11ShaderResourceView 0:Non4K 1:4K
                GL.IssuePluginEvent(SRWorkModule_API.GetRenderEventFunc(), PassThrough.SRWork_PassThrough.Image4kReady ? 1 : 0);

                IntPtr ShaderResourceViewLeft = IntPtr.Zero;
                IntPtr ShaderResourceViewRight = IntPtr.Zero;
                bool SetTexture = true;

                for (int i = 0; i < RingBufferSize; i++)
                {
                    SetTexture &= SRWorkModule_API.GetShaderResourceView(i, PassThrough.SRWork_PassThrough.Image4kReady, ref ShaderResourceViewLeft, ref ShaderResourceViewRight);

                    if (!SetTexture)
                        break;

                    if (UndistortedTextureDataBuffer[i].Initialized)
                        continue;

                    UndistortedTextureDataBuffer[i].Left = Texture2D.CreateExternalTexture(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false, false, ShaderResourceViewLeft);
                    UndistortedTextureDataBuffer[i].Right = Texture2D.CreateExternalTexture(UndistortedImageWidth, UndistortedImageHeight, TextureFormat.RGBA32, false, false, ShaderResourceViewRight);
                    UndistortedTextureDataBuffer[i].Initialized = true;
                }

                SetTextureFromHandleReady = SetTexture;
            }

            // Todo : It will be release when call Release().
            if (SetTextureFromHandleReady)
            {
                TextureUndistortedDisplayLeft = UndistortedTextureDataBuffer[(int)ReceivedBufferIndex].Left;
                TextureUndistortedDisplayRight = UndistortedTextureDataBuffer[(int)ReceivedBufferIndex].Right;
            }

            // Parse the pose.
            if (PassThrough.SRWork_PassThrough.Image4kReady)
            {
                RawUndistortedFrameIndex[0] = PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_frameSeq;
                RawUndistortedTimeIndex[0] = PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_timeStp;
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_left, RawUndistortedPoseLeft, 0, RawUndistortedPoseLeft.Length);
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_4k_data_.output4k_pose_right, RawUndistortedPoseRight, 0, RawUndistortedPoseRight.Length);
            }
            else
            {
                RawUndistortedFrameIndex[0] = PassThrough.SRWork_PassThrough.pass_through_data_.frame_Seq;
                RawUndistortedTimeIndex[0] = PassThrough.SRWork_PassThrough.pass_through_data_.time_Stp;
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_data_.pose_left, RawUndistortedPoseLeft, 0, RawUndistortedPoseLeft.Length);
                Marshal.Copy(PassThrough.SRWork_PassThrough.pass_through_data_.pose_right, RawUndistortedPoseRight, 0, RawUndistortedPoseRight.Length);
            }

            for (int i = 0; i < 4; i++)
            {
                UndistortedPoseLeft.SetColumn(i, new Vector4(RawUndistortedPoseLeft[i * 4 + 0], RawUndistortedPoseLeft[i * 4 + 1],
                                                             RawUndistortedPoseLeft[i * 4 + 2], RawUndistortedPoseLeft[i * 4 + 3]));
                UndistortedPoseRight.SetColumn(i, new Vector4(RawUndistortedPoseRight[i * 4 + 0], RawUndistortedPoseRight[i * 4 + 1],
                                                              RawUndistortedPoseRight[i * 4 + 2], RawUndistortedPoseRight[i * 4 + 3]));
            }
        }
        /// <summary>
        /// Get pass through data for receiving GPU shared textures.
        /// The output depends on whether the non-4k or the 4k pass through is in use.
        /// </summary>
        private static void GetFrameGpuTextureRelatedData(
            bool Use4kPassThrough,
            out uint ReceivedBufferIndex,
            out System.IntPtr ReceivedPointerToSharedHandleLeft,
            out System.IntPtr ReceivedPointerToSharedHandleRight)
        {
            if (Use4kPassThrough)
            {
                ReceivedBufferIndex = PassThrough.SRWork_PassThrough.pass_through_4k_data_.d3d11_texture2d_buffer_index;
                ReceivedPointerToSharedHandleLeft = PassThrough.SRWork_PassThrough.pass_through_4k_data_.d3d11_texture2d_shared_handle_left;
                ReceivedPointerToSharedHandleRight = PassThrough.SRWork_PassThrough.pass_through_4k_data_.d3d11_texture2d_shared_handle_right;
            }
            else
            {
                ReceivedBufferIndex = PassThrough.SRWork_PassThrough.pass_through_data_.d3d11_texture2d_buffer_index;
                ReceivedPointerToSharedHandleLeft = PassThrough.SRWork_PassThrough.pass_through_data_.d3d11_texture2d_shared_handle_left;
                ReceivedPointerToSharedHandleRight = PassThrough.SRWork_PassThrough.pass_through_data_.d3d11_texture2d_shared_handle_right;
            }
        }
        private static void ParseDepthPtrData()
        {
            RawDepthFrameIndex[0] = Depth.SRWork_Depth.depth_data_.frame_seq;
            RawDepthTimeIndex[0] = Depth.SRWork_Depth.depth_data_.time_stp;
            Marshal.Copy(Depth.SRWork_Depth.depth_data_.pose, RawDepthPose, 0, RawDepthPose.Length);

            for (int i = 0; i < 4; i++)
            {
                DepthPose.SetColumn(i, new Vector4(RawDepthPose[i * 4 + 0], RawDepthPose[i * 4 + 1],
                                                   RawDepthPose[i * 4 + 2], RawDepthPose[i * 4 + 3]));
            }
        }
        #endregion

#region Utility
        public static Quaternion Rotation(Matrix4x4 m)
        {
            return Quaternion.LookRotation(m.GetColumn(2), m.GetColumn(1));
        }
        public static Vector3 Position(Matrix4x4 m)
        {
            return new Vector3(m.m03, m.m13, m.m23);
        }
#endregion

#region Depth LinkMethod / Commands / Parameters

        public  static bool IsDepthProcessing { get; private set; }
        public  static int  EnableDepthProcess(bool active)
        {
            int result = (int)Error.FAILED;
            if (active)
            {
                SRWorkModule_API.SetSkipVGAProcess(false);
                result = SRWorkModule_API.LinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.DEPTH);
                if (result == (int)Error.WORK)
                {
                    result = SRWorkModule_API.TurnOnUndistortDataToDepth();
                    IsDepthProcessing = true;
                }
                else IsDepthProcessing = false;
            }
            else
            {
                result = SRWorkModule_API.UnlinkModule((int)ModuleType.PASSTHROUGH, (int)ModuleType.DEPTH);
                if (result == (int)Error.WORK)
                {
                    result = SRWorkModule_API.TurnOffUndistortDataToDepth();
                    IsDepthProcessing = false;
                }
            }
            return result;
        }
        private static bool _IsDepthRefinementEnabled = false;
        public  static bool IsDepthRefinementEnabled 
        { 
            get { return _IsDepthRefinementEnabled; }
            set { if (_IsDepthRefinementEnabled != value) EnableDepthRefinement(value); } 
        }
        public  static int EnableDepthRefinement(bool active)
        {
            int result = SRWorkModule_API.SetDepthParameterBool((int)DepthCmd.ENABLE_REFINEMENT, active);
            if (result == (int)Error.WORK) _IsDepthRefinementEnabled = active;
            return result;
        }
        private static bool _IsDepthEdgeEnhanceEnabled = false;
        public static bool IsDepthEdgeEnhanceEnabled 
        { 
            get { return _IsDepthEdgeEnhanceEnabled; }
            set { if (_IsDepthEdgeEnhanceEnabled != value) EnableDepthEdgeEnhance(value); } 
        }
        public  static int EnableDepthEdgeEnhance(bool active)
        {
            int result = SRWorkModule_API.SetDepthParameterBool((int)DepthCmd.ENABLE_EDGE_ENHANCE, active);
            if (result == (int)Error.WORK) _IsDepthEdgeEnhanceEnabled = active;
            return result;
        }
        private static bool _IsRecordingDatasetEnabled = false;
        public static bool IsRecordingDatasetEnabled
        {
            get { return _IsRecordingDatasetEnabled; }
            set { if (_IsRecordingDatasetEnabled != value) EnableRecordingDataset(value); }
        }
        public static int EnableRecordingDataset(bool active)
        {
            int result = SRWorkModule_API.SetDepthParameterBool((int)DepthCmd.ENABLE_RECORDING_DATASET, active);
            if (result == (int)Error.WORK) _IsRecordingDatasetEnabled = active;
            return result;
        }

        public static DepthCase DepthCase { get; private set; }
        public static int SetDefaultDepthCase(DepthCase depthCase)
        {
            int result = SRWorkModule_API.SetDepthParameterInt((int)DepthParam.DEPTH_USING_CASE, (int)depthCase);
            if (result == (int)Error.WORK) DepthCase = depthCase;
            return result;
        }
        public static int SetDepthCase(DepthCase depthCase)
        {
            int result = SRWorkModule_API.SetDepthParameterInt((int)DepthCmd.CHANGE_DEPTH_CASE, (int)depthCase);
            if (result == (int)Error.WORK) DepthCase = depthCase;
            return result;
        }

        private static float _DepthConfidenceThreshold;
        public  static float  DepthConfidenceThreshold
        {
            get
            {
                SRWorkModule_API.GetDepthParameterFloat((int)DepthParam.CONFIDENCE_THRESHOLD, ref _DepthConfidenceThreshold);
                return _DepthConfidenceThreshold;
            }
            set
            {
                int result = SRWorkModule_API.SetDepthParameterFloat((int)DepthParam.CONFIDENCE_THRESHOLD, value);
                if (result == (int)Error.WORK) _DepthConfidenceThreshold = value;
            }
        }
        private static int _DepthDenoiseGuidedFilter;
        public  static int  DepthDenoiseGuidedFilter
        {
            get
            {
                SRWorkModule_API.GetDepthParameterInt((int)DepthParam.DENOISE_GUIDED_FILTER, ref _DepthDenoiseGuidedFilter);
                return _DepthDenoiseGuidedFilter;
            }
            set
            {
                int result = SRWorkModule_API.SetDepthParameterInt((int)DepthParam.DENOISE_GUIDED_FILTER, value);
                if (result == (int)Error.WORK) _DepthDenoiseGuidedFilter = value;
            }
        }
        private static int _DepthDenoiseMedianFilter;
        public  static int  DepthDenoiseMedianFilter
        {
            get
            {
                SRWorkModule_API.GetDepthParameterInt((int)DepthParam.DENOISE_MEDIAN_FILTER, ref _DepthDenoiseMedianFilter);
                return _DepthDenoiseMedianFilter;
            }
            set
            {
                int result = SRWorkModule_API.SetDepthParameterInt((int)DepthParam.DENOISE_MEDIAN_FILTER, value);
                if (result == (int)Error.WORK) _DepthDenoiseMedianFilter = value;
            }
        }
        #endregion

#region Camera Commands / Parameters
        [StructLayout(LayoutKind.Sequential)]
        public struct CameraSettingInfo
        {
            public Int32 Status;
            public Int32 DefaultValue;
            public Int32 Min;
            public Int32 Max;
            public Int32 Step;
            public Int32 DefaultMode;
            public Int32 Value;
            public Int32 Mode;  // AUTO = 1, MANUAL = 2
        };
        public enum CameraSetting
        {
            BRIGHTNESS              = 100,
            CONTRAST                = 101,
            HUE                     = 102,
            SATURATION              = 103,
            SHARPNESS               = 104,
            GAMMA                   = 105,
            //COLOR_ENABLE          = 106,
            WHITE_BALANCE           = 107,
            BACKLIGHT_COMPENSATION  = 108,
            GAIN                    = 109,
            //PAN                   = 110,
            //TILT                  = 111,
            //ROLL                  = 112,
            //ZOOM                  = 113,
            //EXPOSURE              = 114,
            //IRIS                  = 115,
            //FOCUS                 = 116,
        }
        public static int GetCameraSettingInfo(CameraSetting item, ref CameraSettingInfo paramInfo)
        {
            int result = (int)Error.FAILED;
            result = SRWorkModule_API.GetCameraStatus((int)item, ref paramInfo.Status);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraDefaultValue((int)item, ref paramInfo.DefaultValue);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMin((int)item, ref paramInfo.Min);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMax((int)item, ref paramInfo.Max);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraStep((int)item, ref paramInfo.Step);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraDefaultMode((int)item, ref paramInfo.DefaultMode);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraValue((int)item, ref paramInfo.Value);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.GetCameraMode((int)item, ref paramInfo.Mode);
            if (result != (int)Error.WORK) return result;

            return result;
        }
        public static int SetCameraSettingInfo(CameraSetting item, CameraSettingInfo paramInfo)
        {
            int result = (int)Error.FAILED;
            result = SRWorkModule_API.SetCameraStatus((int)item, paramInfo.Status);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraDefaultValue((int)item, paramInfo.DefaultValue);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMin((int)item, paramInfo.Min);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMax((int)item, paramInfo.Max);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraStep((int)item, paramInfo.Step);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraDefaultMode((int)item, paramInfo.DefaultMode);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraValue((int)item, paramInfo.Value);
            if (result != (int)Error.WORK) return result;
            result = SRWorkModule_API.SetCameraMode((int)item, paramInfo.Mode);
            if (result != (int)Error.WORK) return result;
            return result;
        }
        #endregion
    }
}
