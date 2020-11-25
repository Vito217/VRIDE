using System;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The result of a locomotion request.
    /// </summary>
    /// <seealso cref="LocomotionSystem.RequestExclusiveOperation"/>
    /// <seealso cref="LocomotionSystem.FinishExclusiveOperation"/>
    public enum RequestResult
    {
        /// <summary>
        /// The locomotion request was successful.
        /// </summary>
        Success,

        /// <summary>
        /// The locomotion request failed due to the system being currently busy.
        /// </summary>
        Busy,

        /// <summary>
        /// The locomotion request failed due to an unknown error.
        /// </summary>
        Error,
    }

    /// <summary>
    /// The <see cref="LocomotionSystem"/> object is used to control access to the XR Rig. This system enforces that only one
    /// Locomotion Provider can move the XR Rig at one time. This is the only place that access to an XR Rig is controlled,
    /// having multiple instances of a <see cref="LocomotionSystem"/> drive a single XR Rig is not recommended.
    /// </summary>
    public class LocomotionSystem : MonoBehaviour
    {
        LocomotionProvider m_CurrentExclusiveProvider;
        float m_TimeMadeExclusive;

        [SerializeField]
        [Tooltip("The timeout (in seconds) for exclusive access to the XR Rig.")]
        float m_Timeout = 10f;

        /// <summary>
        /// The timeout (in seconds) for exclusive access to the XR Rig.
        /// </summary>
        public float timeout
        {
            get => m_Timeout;
            set => m_Timeout = value;
        }

        [SerializeField]
        [Tooltip("The XR Rig object to provide access control to.")]
        XRRig m_XRRig;

        /// <summary>
        /// The XR Rig object to provide access control to.
        /// </summary>
        public XRRig xrRig
        {
            get => m_XRRig;
            set => m_XRRig = value;
        }

        /// <summary>
        /// (Read Only) Whether a locomotion request is already being performed.
        /// </summary>
        public bool busy => m_CurrentExclusiveProvider != null;

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("Busy has been deprecated. Use busy instead. (UnityUpgradable) -> busy")]
        public bool Busy => busy;
#pragma warning restore IDE1006

        protected void Awake()
        {
            if (m_XRRig == null)
                m_XRRig = FindObjectOfType<XRRig>();
        }

        protected void Update()
        {
            if (m_CurrentExclusiveProvider != null && Time.time > m_TimeMadeExclusive + m_Timeout)
            {
                ResetExclusivity();
            }
        }

        /// <summary>
        /// Attempt to "lock" access to the XR Rig for the <paramref name="provider"/>.
        /// </summary>
        /// <param name="provider">The locomotion provider that is requesting access.</param>
        /// <returns>Returns a <see cref="RequestResult"/> that reflects the status of the request.</returns>
        public RequestResult RequestExclusiveOperation(LocomotionProvider provider)
        {
            if (provider == null)
                return RequestResult.Error;

            if (m_CurrentExclusiveProvider == null)
            {
                m_CurrentExclusiveProvider = provider;
                m_TimeMadeExclusive = Time.time;
                return RequestResult.Success;
            }

            return m_CurrentExclusiveProvider != provider ? RequestResult.Busy : RequestResult.Error;
        }

        internal void ResetExclusivity()
        {
            m_CurrentExclusiveProvider = null;
            m_TimeMadeExclusive = 0f;
        }

        /// <summary>
        /// Informs the <see cref="LocomotionSystem"/> that exclusive access to the XR Rig is no longer required.
        /// </summary>
        /// <param name="provider">The locomotion provider that is relinquishing access.</param>
        /// <returns>Returns a <see cref="RequestResult"/> that reflects the status of the request.</returns>
        public RequestResult FinishExclusiveOperation(LocomotionProvider provider)
        {
            if(provider == null || m_CurrentExclusiveProvider == null)
                return RequestResult.Error;

            if (m_CurrentExclusiveProvider == provider)
            {
                ResetExclusivity();
                return RequestResult.Success;
            }

            return RequestResult.Error;
        }
    }
}
