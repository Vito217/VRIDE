using System;

namespace UnityEngine.XR.InteractionSubsystems
{
    /// <summary>
    /// Descriptor for the <see cref="XRGestureSubsystem"/> describing capabilities which may vary by implementation.
    /// </summary>
    public sealed class XRGestureSubsystemDescriptor : SubsystemDescriptor<XRGestureSubsystem>
    {
        /// <summary>
        /// Used in conjunction with <see cref="RegisterDescriptor(Cinfo)"/> to register a provider.
        /// This should only be used by subsystem implementors.
        /// </summary>
        public struct Cinfo : IEquatable<Cinfo>
        {
            /// <summary>
            /// The string used to identify this subsystem implementation.
            /// This will be available when enumerating the available descriptors at runtime.
            /// </summary>
            public string id { get; set; }

            /// <summary>
            /// The <c>Type</c> of the implementation.
            /// </summary>
            public Type subsystemImplementationType { get; set; }

            public override int GetHashCode()
            {
                unchecked
                {
                    var hash = (id != null) ? id.GetHashCode() : 0;
                    hash = hash * 486187739 + ((subsystemImplementationType != null) ? subsystemImplementationType.GetHashCode() : 0);
                    return hash;
                }
            }

            public override bool Equals(object obj)
            {
                if (!(obj is Cinfo))
                    return false;

                return Equals((Cinfo)obj);
            }

            public bool Equals(Cinfo other)
            {
                return
                    string.Equals(id, other.id) &&
                    (subsystemImplementationType == other.subsystemImplementationType);
            }

            public static bool operator ==(Cinfo lhs, Cinfo rhs)
            {
                return lhs.Equals(rhs);
            }

            public static bool operator !=(Cinfo lhs, Cinfo rhs)
            {
                return !lhs.Equals(rhs);
            }
        }

        /// <summary>
        /// Register a subsystem implementation.
        /// This should only be used by subsystem implementors.
        /// </summary>
        /// <param name="cinfo">Information used to construct the descriptor.</param>
        public static void RegisterDescriptor(Cinfo cinfo)
        {
            SubsystemRegistration.CreateDescriptor(new XRGestureSubsystemDescriptor(cinfo));
        }

        XRGestureSubsystemDescriptor(Cinfo cinfo)
        {
            id = cinfo.id;
            subsystemImplementationType = cinfo.subsystemImplementationType;
        }
    }
}
