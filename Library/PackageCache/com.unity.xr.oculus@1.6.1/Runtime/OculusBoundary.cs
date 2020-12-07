using UnityEngine;

namespace Unity.XR.Oculus
{
    public static class Boundary
    {
        public enum BoundaryType
        {
            /// <summary>
            ///  Outer Boundary -  axis-aligned rectangular bounding box enclosing the outer boundary.
            /// </summary>
            OuterBoundary = 0,
            /// <summary>
            ///  Play area - axis-aligned smaller rectangle area inside outer boundary where gameplay happens.
            /// </summary>
            PlayArea = 1
        }

        /// <summary>
        /// Returns true if the boundary system is currently configured with valid boundary data.
        /// </summary>
        public static bool GetBoundaryConfigured()
        {
            return NativeMethods.GetBoundaryConfigured();
        }

        /// <summary>
        /// Get a vector of the spatial dimensions of the specified boundary type. (x = width, y = height, z = depth) with height always returning 0.
        /// Return true if boundary dimensions are supported and values are available. Return false otherwise.
        /// </summary>
        public static bool GetBoundaryDimensions(BoundaryType boundaryType, out Vector3 dimensions)
        {
            return NativeMethods.GetBoundaryDimensions(boundaryType, out dimensions);
        }

        /// <summary>
        /// Returns true if the boundary system is currently visible. Return false otherwise.
        /// </summary>
        public static bool GetBoundaryVisible()
        {
            return NativeMethods.GetBoundaryVisible();
        }

        /// <summary>
        /// Requests that the boundary system visibility be set to the specified value.
        /// The actual visibility can be overridden by the system (i.e., proximity trigger) or by the user (boundary system disabled)
        /// </summary>
        public static void SetBoundaryVisible(bool boundaryVisible)
        {
            NativeMethods.SetBoundaryVisible(boundaryVisible);
        }
    }
}
