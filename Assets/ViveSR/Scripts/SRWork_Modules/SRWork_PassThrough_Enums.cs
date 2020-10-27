//========= Copyright 2018, HTC Corporation. All rights reserved. ===========
using System;
using System.Runtime.InteropServices;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace PassThrough
        {
            /** @struct PassThroughData
            * A struct containing all data listed below.
            */
            public struct PassThroughData
            {
                public IntPtr distorted_frame_left;
                public IntPtr distorted_frame_right;
                public IntPtr undistorted_frame_left;
                public IntPtr undistorted_frame_right;
                public int frame_Seq;
                public int time_Stp;
                public IntPtr pose_left;
                public IntPtr pose_right;
                public int lux_left;
                public int lux_right;
                public int color_temperature_left;
                public int color_temperature_right;
                public int exposure_time_left;
                public int exposure_time_right;
                public int analog_gain_left;
                public int analog_gain_right;
                public int digital_gain_left;
                public int digital_gain_right;
                public IntPtr Camera_params;
                public IntPtr d3d11_texture2d_shared_handle_left;
                public IntPtr d3d11_texture2d_shared_handle_right;
                public uint d3d11_texture2d_buffer_index;
                public float gamma;
            };
            /** @struct PassThrough4KData
            * A struct containing all data listed below.
            */
            public struct PassThrough4KData
            {
                public IntPtr distorted_4k_frame_left;
                public IntPtr distorted_4k_frame_right;
                public IntPtr undistorted_4k_frame_left;
                public IntPtr undistorted_4k_frame_right;
                public int output4k_frameSeq;
                public int output4k_timeStp;
                public IntPtr output4k_pose_left;
                public IntPtr output4k_pose_right;
                public int output4k_lux_left;
                public int output4k_lux_right;
                public int output4k_color_temperature_left;
                public int output4k_color_temperature_right;
                public IntPtr Camera4k_params;
                public IntPtr d3d11_texture2d_shared_handle_left;
                public IntPtr d3d11_texture2d_shared_handle_right;
                public uint d3d11_texture2d_buffer_index;
            };
        }
    }
}
