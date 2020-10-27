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
        namespace ControllPose
        {
            public static class SRWork_Controll_Pose
            {
                public static ControllPoseData controller_pose_data_;
                public static float[] pos_left = new float[3];
                public static float[] pos_right = new float[3];
                public static float[] rot_left = new float[4];
                public static float[] rot_right = new float[4];
                private static int LastUpdateResult = (int)Error.FAILED;
                static SRWork_Controll_Pose()
                {
                    controller_pose_data_.pose_position_left = Marshal.AllocCoTaskMem(sizeof(float) * 3);
                    controller_pose_data_.pose_position_right = Marshal.AllocCoTaskMem(sizeof(float) * 3);
                    controller_pose_data_.pose_rotation_left = Marshal.AllocCoTaskMem(sizeof(float) * 4);
                    controller_pose_data_.pose_rotation_right = Marshal.AllocCoTaskMem(sizeof(float) * 4);
                }
                public static bool UpdateData()
                {
                    LastUpdateResult = SRWorkModule_API.GetControllerPoseData(ref controller_pose_data_);
                    Marshal.Copy(controller_pose_data_.pose_position_left, pos_left, 0, pos_left.Length);
                    Marshal.Copy(controller_pose_data_.pose_rotation_left, rot_left, 0, rot_left.Length);
                    Marshal.Copy(controller_pose_data_.pose_position_right, pos_right, 0, pos_right.Length);
                    Marshal.Copy(controller_pose_data_.pose_rotation_right, rot_right, 0, rot_right.Length);
                    return LastUpdateResult == (int)Error.WORK;
                }
            }
        }
    }
}
