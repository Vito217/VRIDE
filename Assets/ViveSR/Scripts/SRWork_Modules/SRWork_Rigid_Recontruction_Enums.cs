//========= Copyright 2018, HTC Corporation. All rights reserved. ===========
using System;
using System.Runtime.InteropServices;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace RigidReconstruction
        {
            /** @struct RigidReconstructionData
            * A struct containing rigidReconstruction data listed below.
            */
            public struct RigidReconstructionData
            {
                public int frame_seq;	        // sizeof(unsigned int)
                public IntPtr posemtx44;               // sizeof(float) * 16
                public int num_vertices;	            // sizeof(int)
                public int bytepervert;	            // sizeof(int)
                public IntPtr vertices;	            // sizeof(float) * 8 * 2500000
                public int num_indices;	            // sizeof(int)
                public IntPtr indices;	                // sizeof(int) * 2500000
                public int cldtype;	                // sizeof(int)
                public int collidernum;	    // sizeof(unsigned int)
                public IntPtr cld_num_verts;	// sizeof(unsigned int) * 200
                public IntPtr cld_numidx;	    // sizeof(unsigned int) * 200
                public IntPtr cld_vertices;	        // sizeof(float) * 3 * 50000
                public IntPtr cld_indices;	            // sizeof(int) * 100000
                public int sector_num;                 // sizeof(int)
                public IntPtr sector_id_list;            // sizeof(int) * 1000000
                public IntPtr sector_vert_num;           // sizeof(int) * 1000000
                public IntPtr sector_idx_num;            // sizeof(int) * 1000000
                public int model_chunk_num;            // sizeof(int)
                public int model_chunk_idx;            // sizeof(int)
            };
        }
    }
}
