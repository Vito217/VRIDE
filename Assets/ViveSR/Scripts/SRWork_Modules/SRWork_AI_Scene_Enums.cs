using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace AIScene
        {
            public struct AISceneData
            {
                public IntPtr frame_left;
                public IntPtr label_left;
                public IntPtr frame_right;
                public IntPtr label_right;
                public int frame_seq;
                public int time_stp;
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
            };
        }
    }
}
