using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Vive
{
    namespace Plugin.SR
    {
        namespace RigidReconstruction
        {
            public static class SRWork_Rigid_Reconstruciton
            {
                public static RigidReconstructionData rigid_reconstruction_data_;
                private static int LastUpdateResult = (int)Error.FAILED;
                public static bool callback;
                private static bool update;
                private static int time_count;
                private static int total_time;
                private static int last_time;
                private static float avg_time;
                public static ViveSR_DataErrorEvent DataErrorEvent = new ViveSR_DataErrorEvent();

                static SRWork_Rigid_Reconstruciton()
                {
                    rigid_reconstruction_data_.posemtx44 = Marshal.AllocCoTaskMem(sizeof(float) * 16);                  // sizeof(float) * 16
                    rigid_reconstruction_data_.vertices = Marshal.AllocCoTaskMem(sizeof(float) * 8 * 2500000);	        // sizeof(float) * 8 * 2500000
                    rigid_reconstruction_data_.indices = Marshal.AllocCoTaskMem(sizeof(int) * 2500000);	                // sizeof(int) * 2500000
                    rigid_reconstruction_data_.cld_num_verts = Marshal.AllocCoTaskMem(sizeof(int) * 200);	            // sizeof(unsigned int) * 200
                    rigid_reconstruction_data_.cld_numidx = Marshal.AllocCoTaskMem(sizeof(int) * 200);	                // sizeof(unsigned int) * 200
                    rigid_reconstruction_data_.cld_vertices = Marshal.AllocCoTaskMem(sizeof(float) * 3 * 50000);	    // sizeof(float) * 3 * 50000
                    rigid_reconstruction_data_.cld_indices = Marshal.AllocCoTaskMem(sizeof(int) * 100000);	            // sizeof(int) * 100000
                    rigid_reconstruction_data_.sector_id_list = Marshal.AllocCoTaskMem(sizeof(int) * 1000000);          // sizeof(int) * 1000000
                    rigid_reconstruction_data_.sector_vert_num = Marshal.AllocCoTaskMem(sizeof(int) * 1000000);         // sizeof(int) * 1000000
                    rigid_reconstruction_data_.sector_idx_num = Marshal.AllocCoTaskMem(sizeof(int) * 1000000);          // sizeof(int) * 1000000
                }
                public static bool UpdateData()
                {
                    LastUpdateResult = SRWorkModule_API.GetRigidReconstructionData(ref rigid_reconstruction_data_);
                    DataErrorEvent.Invoke(LastUpdateResult);
                    return LastUpdateResult == (int)Error.WORK;
                }

                public static int GetLastUpdateError()
                {
                    return LastUpdateResult;
                }

                #region Deprecation period API
                /**
                * @warning The function will remove in the future.
                *   Please use SRWork_Rigid_Reconstruciton.DataErrorEvent.RegisterHandler(int, Action) instead.
                */
                public static void RegisterDataErrorHandler(int errorCode, Action callback)
                {
                    DataErrorEvent.RegisterHandler(errorCode, callback);
                }
                /**
                * @warning The function will remove in the future.
                *   Please use SRWork_Rigid_Reconstruciton.DataErrorEvent.UnregisterHandler(int) instead.
                */
                public static void UnregisterDataErrorHandler(int errorCode)
                {
                    DataErrorEvent.UnregisterHandler(errorCode);
                }
                #endregion
            }
        }
    }
}
