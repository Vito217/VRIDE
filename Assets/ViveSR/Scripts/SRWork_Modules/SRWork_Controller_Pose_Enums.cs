using System;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace ControllPose
        {
            /** @struct DepthMeshData
            * A struct containing all data listed below.
            */
            public struct ControllPoseData
            {
                public uint pose_is_valid_left;            // sizeof(unsigned int) 		
                public uint pose_is_valid_right;           // sizeof(unsigned int) 		
                public IntPtr pose_position_left;            // sizeof(float)*3 		
                public IntPtr pose_position_right;           // sizeof(float)*3 		
                public IntPtr pose_rotation_left;            // sizeof(float)*4 		
                public IntPtr pose_rotation_right;           // sizeof(float)*4 
            }
        }
    }
}
