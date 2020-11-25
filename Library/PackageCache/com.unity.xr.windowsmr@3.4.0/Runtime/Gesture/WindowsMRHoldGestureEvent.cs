using System;
using System.Runtime.InteropServices;
using UnityEngine.Scripting;
using UnityEngine.XR.InteractionSubsystems;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>
    /// The event data related to a WindowsMR Hold gesture
    /// </summary>
    /// <seealso cref="XRGestureSubsystem"/>
    [StructLayout(LayoutKind.Sequential)]
    [Preserve]
    public struct WindowsMRHoldGestureEvent : IEquatable<WindowsMRHoldGestureEvent>
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
        /// Gets a default-initialized <see cref="WindowsMRHoldGestureEvent"/>.
        /// </summary>
        /// <returns>A default <see cref="WindowsMRHoldGestureEvent"/>.</returns>
        public static WindowsMRHoldGestureEvent GetDefault()
        {
            return new WindowsMRHoldGestureEvent(GestureId.invalidId, GestureState.Invalid);
        }

        /// <summary>
        /// Constructs a new <see cref="WindowsMRHoldGestureEvent"/>.
        /// </summary>
        /// <param name="id">The <see cref="GestureId"/> associated with the gesture.</param>
        /// <param name="state">The <see cref="GestureId"/> associated with the gesture.</param>
        public WindowsMRHoldGestureEvent(GestureId id, GestureState state)
        {
            m_Id = id;
            m_State = state;
        }

        /// <summary>
        /// Generates a new string describing the gestures's properties suitable for debugging purposes.
        /// </summary>
        /// <returns>A string describing the gestures's properties.</returns>
        public override string ToString()
        {
            return string.Format(
                "Hold Gesture:\n\tgestureId: {0}\n\tgestureState: {1}",
                id, state);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is WindowsMRHoldGestureEvent && Equals((WindowsMRHoldGestureEvent)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                const int k_HashCodeMultiplier = 486187739;
                var hashCode = m_Id.GetHashCode();
                hashCode = (hashCode * k_HashCodeMultiplier) + ((int)m_State).GetHashCode();
                return hashCode;
            }
        }

        public static bool operator ==(WindowsMRHoldGestureEvent lhs, WindowsMRHoldGestureEvent rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(WindowsMRHoldGestureEvent lhs, WindowsMRHoldGestureEvent rhs)
        {
            return !lhs.Equals(rhs);
        }

        public bool Equals(WindowsMRHoldGestureEvent other)
        {
            return
                m_Id.Equals(other.id) &&
                m_State == other.state;
        }

        GestureId m_Id;
        GestureState m_State;
    }
}
