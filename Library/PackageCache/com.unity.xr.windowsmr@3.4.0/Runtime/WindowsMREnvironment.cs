using System;
using System.Runtime.InteropServices;

using UnityEngine;
using UnityEngine.XR;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// API to get environment specific information that is common or not associated with any one subsystem.
    /// </summary>
    public static class WindowsMREnvironment
    {
        /// <summary>
        ///  Get access to the current HolographicSpace that the plugin is currently running in.
        /// </summary>
        public static IntPtr HolographicSpace
        {
            get
            {
                return Native.GetHolographicSpace();
            }
        }

        /// <summary>
        /// Get the current SpatialCoordinateSystem reference that designates the world origin for the
        /// current runtime environment.
        /// </summary>
        public static IntPtr OriginSpatialCoordinateSystem
        {
            get
            {
                return Native.GetOriginSpatialCoordinateSystem();
            }
        }

        public static IntPtr CurrentHolographicRenderFrame
        {
            get
            {
                return Native.GetCurrentHolographicRenderFrame();
            }
        }

        public static IntPtr CurrentHolographicSimulationFrame
        {
            get
            {
                return Native.GetCurrentHolographicSimulationFrame();
            }
        }
    }
}
