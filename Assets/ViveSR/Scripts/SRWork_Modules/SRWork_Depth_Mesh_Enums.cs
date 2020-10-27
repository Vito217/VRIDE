using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace DepthMesh
        {
            /** @struct DepthMeshData
            * A struct containing all data listed below.
            */
            public struct DepthMeshData
            {
                public int time_stp;                    // sizeof(unsigned int)
                public IntPtr pose;                     // sizeof(float) * 16
                public int num_vertices;                // sizeof(unsigned int)
                public int bytepervert;                 // sizeof(unsigned int)
                public IntPtr vertices;                 // sizeof(float) * 640 * 480 * 3
                public int num_indices;                 // sizeof(unsigned int)
                public IntPtr indices;                  // sizeof(int) * 640 * 480 * 6
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
