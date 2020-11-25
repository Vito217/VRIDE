using System;
using System.Runtime.InteropServices;
using UnityEngine.Scripting;
using UnityEngine.XR.InteractionSubsystems;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// The event data related to a WindowsMR Navigation gesture
    /// </summary>
    /// <seealso cref="XRGestureSubsystem"/>
    [StructLayout(LayoutKind.Sequential)]
    [Preserve]
    public struct WindowsMRNavigationGestureEvent : IEquatable<WindowsMRNavigationGestureEvent>
    {
        /// <summary>
        /// The <see cref="GestureId"/> associated with this gesture.
        /// </summary>
        public GestureId id { get { return m_Id; } }

        /// <summary>
        /// The <see cref="GestureState"/> of the gesture.
        /// </summary>
        public GestureState state { get { return m_State; } }

        /// <summary>
        /// The normalized offset, since the navigation gesture began, of the input within the unit cube for the navigation gesture.
        /// </summary>
        public Vector3 normalizedOffset { get { return m_NormalizedOffset; } }

        /// <summary>
        /// Gets a default-initialized <see cref="WindowsMRNavigationGestureEvent"/>.
        /// </summary>
        /// <returns>A default <see cref="WindowsMRNavigationGestureEvent"/>.</returns>
        public static WindowsMRNavigationGestureEvent GetDefault()
        {
            return new WindowsMRNavigationGestureEvent(GestureId.invalidId, GestureState.Invalid, Vector3.zero);
        }

        /// <summary>
        /// Constructs a new <see cref="WindowsMRNavigationGestureEvent"/>.
        /// </summary>
        /// <param name="id">The <see cref="GestureId"/> associated with the gesture.</param>
        /// <param name="state">The <see cref="GestureId"/> associated with the gesture.</param>
        /// <param name="normalizedOffset">The normalized offset associated with the gesture.</param>
        public WindowsMRNavigationGestureEvent(GestureId id, GestureState state, Vector3 normalizedOffset)
        {
            m_Id = id;
            m_State = state;
            m_NormalizedOffset = normalizedOffset;
        }

        /// <summary>
        /// Generates a new string describing the gestures's properties suitable for debugging purposes.
        /// </summary>
        /// <returns>A string describing the gestures's properties.</returns>
        public override string ToString()
        {
            return string.Format(
                "Navigation Gesture:\n\tgestureId: {0}\n\tgestureState: {1}\n\tnormalizedOffset: {2}",
                id, state, normalizedOffset);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is WindowsMRNavigationGestureEvent && Equals((WindowsMRNavigationGestureEvent)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                const int k_HashCodeMultiplier = 486187739;
                var hashCode = m_Id.GetHashCode();
                hashCode = (hashCode * k_HashCodeMultiplier) + ((int)m_State).GetHashCode();
                hashCode = (hashCode * k_HashCodeMultiplier) + (int)m_NormalizedOffset.GetHashCode();
                return hashCode;
            }
        }

        public static bool operator ==(WindowsMRNavigationGestureEvent lhs, WindowsMRNavigationGestureEvent rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(WindowsMRNavigationGestureEvent lhs, WindowsMRNavigationGestureEvent rhs)
        {
            return !lhs.Equals(rhs);
        }

        public bool Equals(WindowsMRNavigationGestureEvent other)
        {
            return
                m_Id.Equals(other.id) &&
                m_State == other.state &&
                m_NormalizedOffset == other.normalizedOffset;
        }

        GestureId m_Id;
        GestureState m_State;
        Vector3 m_NormalizedOffset;
    }
}
