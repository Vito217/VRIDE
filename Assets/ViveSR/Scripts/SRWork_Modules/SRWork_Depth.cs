using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace Depth
        {
            public static class SRWork_Depth
            {
                public static DepthData depth_data_;
                private static int LastUpdateResult;
                public static int LastUpdateDataResult
                {
                    get { return LastUpdateResult; }
                    private set {}
                }
                public static bool callback;
                private static bool update;
                private static int time_count;
                private static int total_time;
                private static int last_time;
                private static float avg_time;

                static SRWork_Depth()
                {
                    depth_data_.left_frame = Marshal.AllocCoTaskMem(sizeof(char) * 640 * 480 * 4);
                    depth_data_.depth_map = Marshal.AllocCoTaskMem(sizeof(float) * 640 * 480 * 1);
                    depth_data_.pose = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    depth_data_.camera_params = Marshal.AllocCoTaskMem(sizeof(char) * SRWorkModule_API.CAMERA_PARAMS_SIZE);
                }
                public static bool UpdateData()
                {
                    LastUpdateResult = SRWorkModule_API.GetDepthData(ref depth_data_);
                    return LastUpdateResult == (int)Error.WORK;
                }
                private static void DepthDataCallback(IntPtr data)
                {
                    update = UpdateData();
                }

                public static bool UpdateDepthTexture(ref Texture2D depth_texture)
                {
                    if (depth_data_.left_frame == IntPtr.Zero) return false;
                    if (!update)
                        return update;
                    depth_texture.LoadRawTextureData(depth_data_.left_frame, 640 * 480 * 4);
                    depth_texture.Apply();
                    update = false;
                    return !update;
                }
            }
        }
    }
}
