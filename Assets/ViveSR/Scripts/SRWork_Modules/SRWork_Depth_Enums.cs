using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace Depth
        {
            /** @struct DepthData
            * A struct containing all data listed below.
            */
            public struct DepthData
            {
                public IntPtr left_frame;               // sizeof(char) * 640 * 480 * 4
                public IntPtr depth_map;                // sizeof(float) * 640 * 480 * 1
                public int frame_seq;                   // sizeof(unsigned int)
                public int time_stp;                    // sizeof(unsigned int)
                public IntPtr pose;                     // sizeof(float) * 16
                public int lux_left;                    // sizeof(int)
                public int color_temperature_left;      // sizeof(int)
                public int exposure_time_left;          // sizeof(int)
                public int analog_gain_left;            // sizeof(int)
                public int digital_gain_left;           // sizeof(int)
                public IntPtr camera_params;            // sizeof(char) * SRWorkModule_API.CAMERA_PARAMS_SIZE
            }
        }
    }
}
