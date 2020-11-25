using System;
using System.Runtime.InteropServices;

namespace UnityEngine.XR.InteractionSubsystems
{
    /// <summary>
    /// The event data for a common gesture used to activate world geometry or UI.
    /// </summary>
    /// <seealso cref="XRGestureSubsystem"/>
    [StructLayout(LayoutKind.Sequential)]
    public struct ActivateGestureEvent : IEquatable<ActivateGestureEvent>
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
        /// Gets a default-initialized <see cref="ActivateGestureEvent"/>. 
        /// </summary>
        /// <returns>A default <see cref="ActivateGestureEvent"/>.</returns>
        public static ActivateGestureEvent GetDefault()
        {
            return new ActivateGestureEvent(GestureId.invalidId, GestureState.Invalid);
        }

        /// <summary>
        /// Constructs a new <see cref="ActivateGestureEvent"/>.
        /// </summary>
        /// <param name="id">The <see cref="GestureId"/> associated with the gesture.</param>
        /// <param name="state">The <see cref="GestureId"/> associated with the gesture.</param>
        public ActivateGestureEvent(GestureId id, GestureState state)
        {
            m_Id = id;
            m_State = state;
        }

        /// <summary>
        /// Generates a new string describing the gesture's properties suitable for debugging purposes.
        /// </summary>
        /// <returns>A string describing the gesture's properties.</returns>
        public override string ToString()
        {
            return string.Format(
                "Plane:\n\tid: {0}\n\tstate: {1}\n\t",
                id, state);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is ActivateGestureEvent && Equals((ActivateGestureEvent)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = m_Id.GetHashCode();
                hashCode = (hashCode * 486187739) + ((int)m_State).GetHashCode();
                return hashCode;
            }
        }

        public static bool operator ==(ActivateGestureEvent lhs, ActivateGestureEvent rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(ActivateGestureEvent lhs, ActivateGestureEvent rhs)
        {
            return !lhs.Equals(rhs);
        }

        public bool Equals(ActivateGestureEvent other)
        {
            return
                m_Id.Equals(other.id) &&
                m_State == other.state;
        }

        GestureId m_Id;
        GestureState m_State;
    }
}