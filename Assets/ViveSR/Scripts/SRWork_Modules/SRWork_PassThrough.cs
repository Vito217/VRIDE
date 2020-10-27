using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using UnityEngine;
namespace Vive
{
    namespace Plugin.SR
    {
        namespace PassThrough
        {
            public static class SRWork_PassThrough
            {
                public static bool Image4kReady = false;

                //The region of Deprecation period API will remove in the future.
                #region Deprecation period API
                /**
                * The variable b4KImageReady has changed to Image4kReady.
                * @warning The variable will remove in the future.
                */
                public static bool b4KImageReady
                {
                    get { return Image4kReady; }
                    set { Image4kReady = value; }
                }
                #endregion
                public static int DistortedImageWidth = 640, DistortedImageHeight = 480, DistortedImageChannel = 4;
                public static int UndistortedImageWidth = 1150, UndistortedImageHeight = 750, UndistortedImageChannel = 4;
                public static int Distorted4KImageWidth = 1920, Distorted4KImageHeight = 1920, Distorted4KImageChannel = 4;
                public static int Undistorted4KImageWidth = 2424, Undistorted4KImageHeight = 2424, Undistorted4KImageChannel = 4;

                private static int LastUpdateFrame = -1;
                private static int LastUpdateResult = (int)Error.FAILED;
                private static int LastUpdateResult4K = (int)Error.FAILED;
                public static int LastUpdateDataResult
                {
                    get { return Image4kReady ? LastUpdateResult4K : LastUpdateResult; }
                    private set {}
                }
                private static IntPtr PassThroughDataLeftDistort;
                private static IntPtr PassThroughDataRighttDistort;
                public static PassThroughData pass_through_data_;
                public static PassThrough4KData pass_through_4k_data_;
                public static bool callback;
                public static ViveSR_DataErrorEvent DataErrorEvent = new ViveSR_DataErrorEvent();
                public static ViveSR_DataErrorEvent DataErrorEvent4K = new ViveSR_DataErrorEvent();
                static SRWork_PassThrough()
                {
                    SRWorkModule_API.GetPassThrougParameterBool((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_4K_READY, ref Image4kReady);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_DISTORTED_WIDTH, ref DistortedImageWidth);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_DISTORTED_HEIGHT, ref DistortedImageHeight);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_DISTORTED_CHANNEL, ref DistortedImageChannel);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_UNDISTORTED_WIDTH, ref UndistortedImageWidth);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_UNDISTORTED_HEIGHT, ref UndistortedImageHeight);

                    SRWorkModule_API.GetPassThrougParameterInt((int)Vive.Plugin.SR.PassThroughParam.OUTPUT_UNDISTORTED_CHANNEL, ref UndistortedImageChannel);
                    //b4KImageReady = false;
                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_DISTORTED_4K_WIDTH, ref Distorted4KImageWidth);

                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_DISTORTED_4K_HEIGHT, ref Distorted4KImageHeight);

                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_DISTORTED_4K_CHANNEL, ref Distorted4KImageChannel);

                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_UNDISTORTED_4K_WIDTH, ref Undistorted4KImageWidth);

                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_UNDISTORTED_4K_HEIGHT, ref Undistorted4KImageHeight);

                    SRWorkModule_API.GetPassThroug4KParameterInt((int)Vive.Plugin.SR.PassThrough4KParam.OUTPUT_UNDISTORTED_4K_CHANNEL, ref Undistorted4KImageChannel);

                    //IntPtr.Zero; // 
                    pass_through_data_.distorted_frame_left = IntPtr.Zero;
                    pass_through_data_.distorted_frame_right = IntPtr.Zero;
                    pass_through_data_.undistorted_frame_left = IntPtr.Zero;
                    pass_through_data_.undistorted_frame_right = IntPtr.Zero;

                    PassThroughDataLeftDistort = Marshal.AllocCoTaskMem(sizeof(char) * DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);
                    PassThroughDataRighttDistort = Marshal.AllocCoTaskMem(sizeof(char) * DistortedImageWidth * DistortedImageHeight * DistortedImageChannel);

                    pass_through_data_.pose_left = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    pass_through_data_.pose_right = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    pass_through_data_.Camera_params = Marshal.AllocCoTaskMem(sizeof(char) * SRWorkModule_API.CAMERA_PARAMS_SIZE);

                    pass_through_data_.d3d11_texture2d_shared_handle_left = Marshal.AllocCoTaskMem(IntPtr.Size);
                    pass_through_data_.d3d11_texture2d_shared_handle_right = Marshal.AllocCoTaskMem(IntPtr.Size);

                    pass_through_4k_data_.distorted_4k_frame_left = IntPtr.Zero;
                    pass_through_4k_data_.distorted_4k_frame_right = IntPtr.Zero;
                    pass_through_4k_data_.undistorted_4k_frame_left = IntPtr.Zero;
                    pass_through_4k_data_.undistorted_4k_frame_right = IntPtr.Zero;

                    pass_through_4k_data_.output4k_pose_left = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    pass_through_4k_data_.output4k_pose_right = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    pass_through_4k_data_.Camera4k_params = Marshal.AllocCoTaskMem(sizeof(char) * SRWorkModule_API.CAMERA_PARAMS_SIZE);

                    pass_through_4k_data_.d3d11_texture2d_shared_handle_left = Marshal.AllocCoTaskMem(IntPtr.Size);
                    pass_through_4k_data_.d3d11_texture2d_shared_handle_right = Marshal.AllocCoTaskMem(IntPtr.Size);

                    //RegisterDistortedCallback();
                }
                public static int SkipVGAPassThrough(bool skip)
                {
                    return SRWorkModule_API.SetSkipVGAProcess(skip);
                }
                public static bool UpdateData()
                {
                    if (Time.frameCount == LastUpdateFrame)
                    {
                        return LastUpdateResult == (int)Error.WORK;
                    }
                    else
                    {
                        LastUpdateFrame = Time.frameCount;
                    }

                    if (Image4kReady)
                    {
                        LastUpdateResult4K = SRWorkModule_API.GetPassThrough4KData(ref pass_through_4k_data_);
                        if (Time.frameCount == 1) LastUpdateResult4K = (int)Error.NO_NEW_DATA;
                        DataErrorEvent4K.Invoke(LastUpdateResult4K);
                        return LastUpdateResult4K == (int)Error.WORK;
                    }
                    else
                    {
                        LastUpdateResult = SRWorkModule_API.GetPassThroughData(ref pass_through_data_);
                        if (Time.frameCount == 1) LastUpdateResult = (int)Error.NO_NEW_DATA;
                        DataErrorEvent.Invoke(LastUpdateResult);
                        return LastUpdateResult == (int)Error.WORK;
                    }
                }

                public static bool TurnOnPassThroughDistortData()
                {
                    int result = SRWorkModule_API.TurnOnDistortData();
                    if (result != (int)Error.WORK)
                        return false;
                    pass_through_data_.distorted_frame_left = PassThroughDataLeftDistort;
                    pass_through_data_.distorted_frame_right = PassThroughDataRighttDistort;
                    return true;
                }

                public static bool TurnOffPassThroughDistortData()
                {
                    int result = SRWorkModule_API.TurnOffDistortData();
                    if (result != (int)Error.WORK)
                        return false;
                    pass_through_data_.distorted_frame_left = IntPtr.Zero;
                    pass_through_data_.distorted_frame_right = IntPtr.Zero;
                    return true;
                }

                /// <summary>
                /// Update given texture2D with camera image.
                /// </summary>
                /// <param name="texture">A texture2D sholud be defined as Texture2D(ImageWidth, ImageHeight, TextureFormat.RGBA32, true/false)</param>
                /// <returns>true if there is any new data.</returns>
                /// 

                private static void DistortedDataCallback(IntPtr data)
                {
                    //update = UpdateData();
                    //TextureDistortedLeft.LoadRawTextureData(data, 612 * 460 * 4);
                    LastUpdateResult = SRWorkModule_API.GetPassThroughData(ref pass_through_data_);
                    //int offset = sizeof(char) * 612 * 460 * 4 * 2 + sizeof(char) * 1150 * 750 * 4 * 2;
                    //undistorted_left_texture.LoadRawTextureData(data, UndistortedImageWidth * UndistortedImageHeight * UndistortedImageChannel);
                    //offset = offset / sizeof(Int32);
                    ////int size = sizeof(char) * 612 * 460 * 4 * 2 + sizeof(char) * 1150 * 750 * 4 * 2 +
                    ////           sizeof(float) * 16 * 2 + sizeof(int) * 12;
                    ////byte[] data_byte = new byte[size];
                    //IntPtr new_ptr;
                    //new_ptr = new IntPtr(data.ToInt32() + offset);
                    //if (IntPtr.Size == sizeof(Int32))
                    //{
                    //    Debug.Log("32");
                    //    new_ptr = new IntPtr(data.ToInt32() + offset);
                    //}
                    //else
                    //{
                    //    Debug.Log("64");
                    //    new_ptr = new IntPtr(data.ToInt64() + offset);
                    //}

                    //Marshal.Copy(new_ptr, frame, 0, frame.Length);
                    Debug.Log("here");
                }

            }
        }
    }
}
