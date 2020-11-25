using System;

namespace UnityEngine.XR.InteractionSubsystems
{
    ///<summary>
    /// The state of a given gesture.  
    /// A gesture can be thought of as a state machine that transitions through multiple states during it's lifetime.
    ///</summary>
    ///<description>
    /// Gestures are Started, then Updated while active, then either Completed or Canceled.  
    /// Some gestures however don't have a continuous lifetime, and instead have a single Discrete state.
    ///</description>
    public enum GestureState
    {
        ///<summary>
        /// This state describes when the user cancels the gesture. Unity considers this gesture to be finished and no longer receives data for it.
        ///</summary>
        Canceled,

        ///<summary>
        /// This state describes when the user completes the gesture. Unity considers this gesture to be finished and no longer receives data for it.
        ///</summary>
        Completed,

        ///<summary>
        /// This state describes when a gesture with a single discrete event occurs.  There will be no more events triggered for this gesture.
        ///</summary>
        Discrete,

        ///<summary>
        /// This state describes when a gesture has been started.  The gesture may be updated before completing or being cancelled.
        ///</summary>
        Started,

        ///<summary>
        /// This state describes when a gesture is being updated during it's lifetime.
        ///</summary>
        Updated,

        ///<summary>
        /// This state describes an invalid gesture. This gesture is no longer valid or hasn't been setup.
        ///</summary>
        Invalid
    }
}