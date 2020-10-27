using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace DepthMesh
        {
            public static class SRWork_Depth_Mesh
            {
                public static DepthMeshData depth_mesh_data_;
                private static int LastUpdateResult;
                public static bool callback;
                private static bool update;
                private static int time_count;
                private static int total_time;
                private static int last_time;
                private static float avg_time;

                static SRWork_Depth_Mesh()
                {
                    depth_mesh_data_.pose = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    depth_mesh_data_.camera_params = Marshal.AllocCoTaskMem(sizeof(char) * SRWorkModule_API.CAMERA_PARAMS_SIZE);
                    depth_mesh_data_.vertices = Marshal.AllocCoTaskMem(sizeof(float) * 640 * 480 * 3);
                    depth_mesh_data_.indices = Marshal.AllocCoTaskMem(sizeof(int) * 640 * 480 * 6);
                }
                public static bool UpdateData()
                {
                    LastUpdateResult = SRWorkModule_API.GetDepthMeshData(ref depth_mesh_data_);
                    return LastUpdateResult == (int)Error.WORK;
                }
            }
        }
    }
}
