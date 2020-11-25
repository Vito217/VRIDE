using System;
using System.Runtime.InteropServices;
using UnityEngine.Scripting;
using UnityEngine.XR.InteractionSubsystems;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// The event data related to a WindowsMR Tapped gesture
    /// </summary>
    /// <seealso cref="XRGestureSubsystem"/>
    [StructLayout(LayoutKind.Sequential)]
    [Preserve]
    public struct WindowsMRTappedGestureEvent : IEquatable<WindowsMRTappedGestureEvent>
    {
        /// <summary>
        /// The <see cref="GestureId"/> associated with this gesture.
        /// </summary>
        public GestureId id { get { return m_Id; } }

        /// <summary>
        /// The <see cref="state"/> of the gesture.
        /// </summary>
        public GestureState state { get { return m_State; } }

        /// <summary>
        /// The <see cref="tappedCount"/> of the gesture.
        /// </summary>
        public int tappedCount { get { return m_TappedCount; } }

        /// <summary>
        /// Gets a default-initialized <see cref="WindowsMRTappedGestureEvent"/>.
        /// </summary>
        /// <returns>A default <see cref="WindowsMRTappedGestureEvent"/>.</returns>
        public static WindowsMRTappedGestureEvent GetDefault()
        {
            return new WindowsMRTappedGestureEvent(GestureId.invalidId, GestureState.Invalid, 0);
        }

        /// <summary>
        /// Constructs a new <see cref="WindowsMRTappedGestureEvent"/>.
        /// </summary>
        /// <param name="id">The <see cref="GestureId"/> associated with the gesture.</param>
        /// <param name="state">The <see cref="GestureState"/> associated with the gesture.</param>
        /// <param name="tappedCount">The <see cref="int"/> associated with the gesture.</param>
        public WindowsMRTappedGestureEvent(GestureId id, GestureState state, int tappedCount)
        {
            m_Id = id;
            m_State = state;
            m_TappedCount = tappedCount;
        }

        /// <summary>
        /// Generates a new string describing the gestures's properties suitable for debugging purposes.
        /// </summary>
        /// <returns>A string describing the gestures's properties.</returns>
        public override string ToString()
        {
            return string.Format(
                "Tapped Gesture:\n\tgestureId: {0}\n\tgestureState: {1}\n\ttappedCount: {2}",
                id, state, tappedCount);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is WindowsMRTappedGestureEvent && Equals((WindowsMRTappedGestureEvent)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                const int k_HashCodeMultiplier = 486187739;
                var hashCode = m_Id.GetHashCode();
                hashCode = (hashCode * k_HashCodeMultiplier) + ((int)m_State).GetHashCode();
                hashCode = (hashCode * k_HashCodeMultiplier) + m_TappedCount.GetHashCode();
                return hashCode;
            }
        }

        public static bool operator ==(WindowsMRTappedGestureEvent lhs, WindowsMRTappedGestureEvent rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(WindowsMRTappedGestureEvent lhs, WindowsMRTappedGestureEvent rhs)
        {
            return !lhs.Equals(rhs);
        }

        public bool Equals(WindowsMRTappedGestureEvent other)
        {
            return
                m_Id.Equals(other.id) &&
                m_State == other.state &&
                m_TappedCount == other.tappedCount;
        }

        GestureId m_Id;
        GestureState m_State;
        int m_TappedCount;
    }
}
