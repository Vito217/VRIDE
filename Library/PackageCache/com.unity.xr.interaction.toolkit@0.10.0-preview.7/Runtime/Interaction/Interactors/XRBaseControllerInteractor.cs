using System;
using System.Collections.Generic;
using UnityEngine.Serialization;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Abstract base class from which all interactors that are controller driven derive.
    /// This class hooks into the interaction system (via <see cref="XRInteractionManager"/>) and provides base virtual methods for handling
    /// hover and selection. Additionally, this class provides functionality for checking the controller's selection status
    /// and hiding the controller on selection.
    /// </summary>
    public abstract class XRBaseControllerInteractor : XRBaseInteractor
    {
        public enum InputTriggerType
        {
            State,
            StateChange,
            // This interaction will start on the select button/key being activated
            // and remain engaged until the second time it is activated.
            Toggle,
            // Start interaction on select enter, and wait until the second time the
            // select key is depressed before exiting the interaction.  This is useful
            // for grabbing and throwing without having to hold a button down.
            Sticky,
        }

        [SerializeField]
        InputTriggerType m_SelectActionTrigger = InputTriggerType.State;
        /// <summary>
        /// Controls whether this interactor toggles selection on button press (rather than selection on press).
        /// </summary>
        public InputTriggerType selectActionTrigger
        {
            get => m_SelectActionTrigger;
            set => m_SelectActionTrigger = value;
        }

        [SerializeField]
        bool m_HideControllerOnSelect;
        /// <summary>
        /// Controls whether this interactor should hide the controller on selection.
        /// </summary>
        public bool hideControllerOnSelect
        {
            get => m_HideControllerOnSelect;
            set
            {
                m_HideControllerOnSelect = value;
                if (!m_HideControllerOnSelect && m_Controller != null)
                    m_Controller.hideControllerModel = false;
            }
        }

        [SerializeField, FormerlySerializedAs("m_PlayAudioClipOnSelectEnter")]
        bool m_PlayAudioClipOnSelectEntered;
        /// <summary>
        /// Controls whether to play an <see cref="AudioClip"/> on Select Entered.
        /// </summary>
        /// <seealso cref="audioClipForOnSelectEntered"/>
        public bool playAudioClipOnSelectEntered
        {
            get => m_PlayAudioClipOnSelectEntered;
            set => m_PlayAudioClipOnSelectEntered = value;
        }

        [Obsolete("playAudioClipOnSelectEnter has been deprecated. Use playAudioClipOnSelectEntered instead. (UnityUpgradable) -> playAudioClipOnSelectEntered")]
        public bool playAudioClipOnSelectEnter => playAudioClipOnSelectEntered;

        [SerializeField, FormerlySerializedAs("m_AudioClipForOnSelectEnter")]
        AudioClip m_AudioClipForOnSelectEntered;
        /// <summary>
        /// The <see cref="AudioClip"/> to play on Select Entered.
        /// </summary>
        /// <seealso cref="playAudioClipOnSelectEntered"/>
        public AudioClip audioClipForOnSelectEntered
        {
            get => m_AudioClipForOnSelectEntered;
            set => m_AudioClipForOnSelectEntered = value;
        }

        [Obsolete("audioClipForOnSelectEnter has been deprecated. Use audioClipForOnSelectEntered instead. (UnityUpgradable) -> audioClipForOnSelectEntered")]
        public AudioClip audioClipForOnSelectEnter => audioClipForOnSelectEntered;

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("AudioClipForOnSelectEnter has been deprecated. Use audioClipForOnSelectEntered instead. (UnityUpgradable) -> audioClipForOnSelectEntered")]
        public AudioClip AudioClipForOnSelectEnter
        {
            get => audioClipForOnSelectEntered;
            set => audioClipForOnSelectEntered = value;
        }
#pragma warning restore IDE1006

        [SerializeField, FormerlySerializedAs("m_PlayAudioClipOnSelectExit")]
        bool m_PlayAudioClipOnSelectExited;
        /// <summary>
        /// Controls whether to play an <see cref="AudioClip"/> on Select Exited.
        /// </summary>
        /// <seealso cref="audioClipForOnSelectExited"/>
        public bool playAudioClipOnSelectExited
        {
            get => m_PlayAudioClipOnSelectExited;
            set => m_PlayAudioClipOnSelectExited = value;
        }

        [Obsolete("playAudioClipOnSelectExit has been deprecated. Use playAudioClipOnSelectExited instead. (UnityUpgradable) -> playAudioClipOnSelectExited")]
        public bool playAudioClipOnSelectExit => playAudioClipOnSelectExited;

        [SerializeField, FormerlySerializedAs("m_AudioClipForOnSelectExit")]
        AudioClip m_AudioClipForOnSelectExited;
        /// <summary>
        /// The <see cref="AudioClip"/> to play on Select Exited.
        /// </summary>
        /// <seealso cref="playAudioClipOnSelectExited"/>
        public AudioClip audioClipForOnSelectExited
        {
            get => m_AudioClipForOnSelectExited;
            set => m_AudioClipForOnSelectExited = value;
        }

        [Obsolete("audioClipForOnSelectExit has been deprecated. Use audioClipForOnSelectExited instead. (UnityUpgradable) -> audioClipForOnSelectExited")]
        public AudioClip audioClipForOnSelectExit => audioClipForOnSelectExited;

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("AudioClipForOnSelectExit has been deprecated. Use audioClipForOnSelectExited instead. (UnityUpgradable) -> audioClipForOnSelectExited")]
        public AudioClip AudioClipForOnSelectExit
        {
            get => audioClipForOnSelectExited;
            set => audioClipForOnSelectExited = value;
        }
#pragma warning restore IDE1006

        [SerializeField, FormerlySerializedAs("m_PlayAudioClipOnHoverEnter")]
        bool m_PlayAudioClipOnHoverEntered;
        /// <summary>
        /// Controls whether to play an <see cref="AudioClip"/> on Hover Entered.
        /// </summary>
        /// <seealso cref="audioClipForOnHoverEntered"/>
        public bool playAudioClipOnHoverEntered
        {
            get => m_PlayAudioClipOnHoverEntered;
            set => m_PlayAudioClipOnHoverEntered = value;
        }

        [Obsolete("playAudioClipOnHoverEnter has been deprecated. Use playAudioClipOnHoverEntered instead. (UnityUpgradable) -> playAudioClipOnHoverEntered")]
        public bool playAudioClipOnHoverEnter => playAudioClipOnHoverEntered;

        [SerializeField, FormerlySerializedAs("m_AudioClipForOnHoverEnter")]
        AudioClip m_AudioClipForOnHoverEntered;
        /// <summary>
        /// The <see cref="AudioClip"/> to play on Hover Entered.
        /// </summary>
        /// <seealso cref="playAudioClipOnHoverEntered"/>
        public AudioClip audioClipForOnHoverEntered
        {
            get => m_AudioClipForOnHoverEntered;
            set => m_AudioClipForOnHoverEntered = value;
        }

        [Obsolete("audioClipForOnHoverEnter has been deprecated. Use audioClipForOnHoverEntered instead. (UnityUpgradable) -> audioClipForOnHoverEntered")]
        public AudioClip audioClipForOnHoverEnter => audioClipForOnHoverEntered;

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("AudioClipForOnHoverEnter has been deprecated. Use audioClipForOnHoverEntered instead. (UnityUpgradable) -> audioClipForOnHoverEntered")]
        public AudioClip AudioClipForOnHoverEnter
        {
            get => audioClipForOnHoverEntered;
            set => audioClipForOnHoverEntered = value;
        }
#pragma warning restore IDE1006

        [SerializeField, FormerlySerializedAs("m_PlayAudioClipOnHoverExit")]
        bool m_PlayAudioClipOnHoverExited;
        /// <summary>
        /// Controls whether to play an <see cref="AudioClip"/> on Hover Exited.
        /// </summary>
        /// <seealso cref="audioClipForOnHoverExited"/>
        public bool playAudioClipOnHoverExited
        {
            get => m_PlayAudioClipOnHoverExited;
            set => m_PlayAudioClipOnHoverExited = value;
        }

        [Obsolete("playAudioClipOnHoverExit has been deprecated. Use playAudioClipOnHoverExited instead. (UnityUpgradable) -> playAudioClipOnHoverExited")]
        public bool playAudioClipOnHoverExit => playAudioClipOnHoverExited;

        [SerializeField, FormerlySerializedAs("m_AudioClipForOnHoverExit")]
        AudioClip m_AudioClipForOnHoverExited;
        /// <summary>
        /// The <see cref="AudioClip"/> to play on Hover Exited.
        /// </summary>
        /// <seealso cref="playAudioClipOnHoverExited"/>
        public AudioClip audioClipForOnHoverExited
        {
            get => m_AudioClipForOnHoverExited;
            set => m_AudioClipForOnHoverExited = value;
        }

        [Obsolete("audioClipForOnHoverExit has been deprecated. Use audioClipForOnHoverExited instead. (UnityUpgradable) -> audioClipForOnHoverExited")]
        public AudioClip audioClipForOnHoverExit => audioClipForOnHoverExited;

#pragma warning disable IDE1006 // Naming Styles
        [Obsolete("AudioClipForOnHoverExit has been deprecated. Use audioClipForOnHoverExited instead. (UnityUpgradable) -> audioClipForOnHoverExited")]
        public AudioClip AudioClipForOnHoverExit
        {
            get => audioClipForOnHoverExited;
            set => audioClipForOnHoverExited = value;
        }
#pragma warning restore IDE1006

        [SerializeField, FormerlySerializedAs("m_PlayHapticsOnSelectEnter")]
        bool m_PlayHapticsOnSelectEntered;
        /// <summary>
        /// Controls whether to play haptics on Select Entered.
        /// </summary>
        /// <seealso cref="hapticSelectEnterIntensity"/>
        /// <seealso cref="hapticSelectEnterDuration"/>
        public bool playHapticsOnSelectEntered
        {
            get => m_PlayHapticsOnSelectEntered;
            set => m_PlayHapticsOnSelectEntered = value;
        }

        [Obsolete("playHapticsOnSelectEnter has been deprecated. Use playHapticsOnSelectEntered instead. (UnityUpgradable) -> playHapticsOnSelectEntered")]
        public bool playHapticsOnSelectEnter => playHapticsOnSelectEntered;

        [SerializeField]
        [Range(0,1)]
        float m_HapticSelectEnterIntensity;
        /// <summary>
        /// The Haptics intensity to play on Select Entered.
        /// </summary>
        /// <seealso cref="hapticSelectEnterDuration"/>
        /// <seealso cref="playHapticsOnSelectEntered"/>
        public float hapticSelectEnterIntensity
        {
            get => m_HapticSelectEnterIntensity;
            set => m_HapticSelectEnterIntensity= value;
        }

        [SerializeField]
        float m_HapticSelectEnterDuration;
        /// <summary>
        /// The Haptics duration (in seconds) to play on Select Entered.
        /// </summary>
        /// <seealso cref="hapticSelectEnterIntensity"/>
        /// <seealso cref="playHapticsOnSelectEntered"/>
        public float hapticSelectEnterDuration
        {
            get => m_HapticSelectEnterDuration;
            set => m_HapticSelectEnterDuration = value;
        }

        [SerializeField, FormerlySerializedAs("m_PlayHapticsOnSelectExit")]
        bool m_PlayHapticsOnSelectExited;
        /// <summary>
        /// Controls whether to play haptics on Select Exited.
        /// </summary>
        /// <seealso cref="hapticSelectExitIntensity"/>
        /// <seealso cref="hapticSelectExitDuration"/>
        public bool playHapticsOnSelectExited
        {
            get => m_PlayHapticsOnSelectExited;
            set => m_PlayHapticsOnSelectExited = value;
        }

        [Obsolete("playHapticsOnSelectExit has been deprecated. Use playHapticsOnSelectExited instead. (UnityUpgradable) -> playHapticsOnSelectExited")]
        public bool playHapticsOnSelectExit => playHapticsOnSelectExited;

        [SerializeField]
        [Range(0,1)]
        float m_HapticSelectExitIntensity;
        /// <summary>
        /// The Haptics intensity to play on Select Exited.
        /// </summary>
        /// <seealso cref="hapticSelectExitDuration"/>
        /// <seealso cref="playHapticsOnSelectExited"/>
        public float hapticSelectExitIntensity
        {
            get => m_HapticSelectExitIntensity;
            set => m_HapticSelectExitIntensity= value;
        }

        [SerializeField]
        float m_HapticSelectExitDuration;
        /// <summary>
        /// The Haptics duration (in seconds) to play on Select Exited.
        /// </summary>
        /// <seealso cref="hapticSelectExitIntensity"/>
        /// <seealso cref="playHapticsOnSelectExited"/>
        public float hapticSelectExitDuration
        {
            get => m_HapticSelectExitDuration;
            set => m_HapticSelectExitDuration = value;
        }

        [SerializeField, FormerlySerializedAs("m_PlayHapticsOnHoverEnter")]
        bool m_PlayHapticsOnHoverEntered;
        /// <summary>
        /// Controls whether to play haptics on Hover Entered.
        /// </summary>
        /// <seealso cref="hapticHoverEnterIntensity"/>
        /// <seealso cref="hapticHoverEnterDuration"/>
        public bool playHapticsOnHoverEntered
        {
            get => m_PlayHapticsOnHoverEntered;
            set => m_PlayHapticsOnHoverEntered = value;
        }

        [Obsolete("playHapticsOnHoverEnter has been deprecated. Use playHapticsOnHoverEntered instead. (UnityUpgradable) -> playHapticsOnHoverEntered")]
        public bool playHapticsOnHoverEnter => playHapticsOnHoverEntered;

        [SerializeField]
        [Range(0,1)]
        float m_HapticHoverEnterIntensity;
        /// <summary>
        /// The Haptics intensity to play on Hover Entered.
        /// </summary>
        /// <seealso cref="hapticHoverEnterDuration"/>
        /// <seealso cref="playHapticsOnHoverEntered"/>
        public float hapticHoverEnterIntensity
        {
            get => m_HapticHoverEnterIntensity;
            set => m_HapticHoverEnterIntensity = value;
        }

        [SerializeField]
        float m_HapticHoverEnterDuration;
        /// <summary>
        /// The Haptics duration (in seconds) to play on Hover Entered.
        /// </summary>
        /// <seealso cref="hapticHoverEnterIntensity"/>
        /// <seealso cref="playHapticsOnHoverEntered"/>
        public float hapticHoverEnterDuration
        {
            get => m_HapticHoverEnterDuration;
            set => m_HapticHoverEnterDuration = value;
        }

        [SerializeField, FormerlySerializedAs("m_PlayHapticsOnHoverExit")]
        bool m_PlayHapticsOnHoverExited;
        /// <summary>
        /// Controls whether to play haptics on Hover Exited.
        /// </summary>
        /// <seealso cref="hapticHoverExitIntensity"/>
        /// <seealso cref="hapticHoverExitDuration"/>
        public bool playHapticsOnHoverExited
        {
            get => m_PlayHapticsOnHoverExited;
            set => m_PlayHapticsOnHoverExited = value;
        }

        [SerializeField]
        [Range(0,1)]
        float m_HapticHoverExitIntensity;
        /// <summary>
        /// The Haptics intensity to play on Hover Exited.
        /// </summary>
        /// <seealso cref="hapticHoverExitDuration"/>
        /// <seealso cref="playHapticsOnHoverExited"/>
        public float hapticHoverExitIntensity
        {
            get => m_HapticHoverExitIntensity;
            set => m_HapticHoverExitIntensity = value;
        }

        [SerializeField]
        float m_HapticHoverExitDuration;
        /// <summary>
        /// The Haptics duration (in seconds) to play on Hover Exited.
        /// </summary>
        /// <seealso cref="hapticHoverExitIntensity"/>
        /// <seealso cref="playHapticsOnHoverExited"/>
        public float hapticHoverExitDuration
        {
            get => m_HapticHoverExitDuration;
            set => m_HapticHoverExitDuration = value;
        }

        XRBaseController m_Controller;
        public XRBaseController xrController
        {
            get => m_Controller;
            set => m_Controller = value;
        }

        /// <summary>
        /// (Read Only) A list of targets that can be selected.
        /// </summary>
        protected abstract List<XRBaseInteractable> validTargets { get; }

        bool m_ToggleSelectActive;
        bool m_WaitingForSecondDeactivate;
        AudioSource m_EffectsAudioSource;

        /// <inheritdoc />
        protected override void Awake()
        {
            base.Awake();

            // Setup interaction controller (for sending down selection state and input)
            m_Controller = GetComponent<XRBaseController>();
            if (m_Controller == null)
                Debug.LogWarning($"Could not find {nameof(XRBaseController)} component on {gameObject}.", this);

            // If we are toggling selection and have a starting object, start out holding it
            if (m_SelectActionTrigger == InputTriggerType.Toggle && startingSelectedInteractable != null)
                m_ToggleSelectActive = true;

            if (m_PlayAudioClipOnSelectEntered || m_PlayAudioClipOnSelectExited ||
                m_PlayAudioClipOnHoverEntered || m_PlayAudioClipOnHoverExited )
            {
                CreateEffectsAudioSource();
            }
        }

        void CreateEffectsAudioSource()
        {
            m_EffectsAudioSource = gameObject.AddComponent<AudioSource>();
            m_EffectsAudioSource.loop = false;
            m_EffectsAudioSource.playOnAwake = false;
        }

        /// <inheritdoc />
        public override void ProcessInteractor(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            // Perform toggling of selection state
            // and activation of selected object on activate.
            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic)
            {
                if (m_Controller != null)
                {
                    if (m_Controller.selectInteractionState.activatedThisFrame)
                    {
                        if (m_ToggleSelectActive && m_SelectActionTrigger == InputTriggerType.Sticky)
                            m_WaitingForSecondDeactivate = true;

                        if (m_ToggleSelectActive || validTargets.Count > 0)
                            m_ToggleSelectActive = !m_ToggleSelectActive;
                    }

                    if (m_Controller.selectInteractionState.deactivatedThisFrame && m_WaitingForSecondDeactivate)
                        m_WaitingForSecondDeactivate = false;

                    if (selectTarget && m_Controller.activateInteractionState.activatedThisFrame)
                        selectTarget.OnActivate(this);
                    if (selectTarget && m_Controller.activateInteractionState.deactivatedThisFrame)
                        selectTarget.OnDeactivate(this);
                }
            }
        }

        /// <summary>
        /// Gets whether the selection state is active for this interactor.  This will check if the controller has a valid selection
        /// state or whether toggle selection is currently on and active.
        /// </summary>
        public override bool isSelectActive
        {
            get
            {
                if (!base.isSelectActive)
                    return false;

                if (isPerformingManualInteraction)
                    return true;

                switch (m_SelectActionTrigger)
                {
                    case InputTriggerType.State:
                        return m_Controller != null && m_Controller.selectInteractionState.active;

                    case InputTriggerType.StateChange:
                        return (m_Controller != null && m_Controller.selectInteractionState.activatedThisFrame)
                            || (selectTarget != null && !(m_Controller == null || m_Controller.selectInteractionState.deactivatedThisFrame));

                    case InputTriggerType.Toggle:
                        return m_ToggleSelectActive;

                    case InputTriggerType.Sticky:
                        return m_ToggleSelectActive || m_WaitingForSecondDeactivate;

                    default:
                        return false;
                }
            }
        }

        protected bool isUISelectActive => m_Controller != null && m_Controller.uiPressInteractionState.active;

        /// <inheritdoc />
        protected internal override void OnSelectEntering(XRBaseInteractable interactable)
        {
            base.OnSelectEntering(interactable);

            if (m_HideControllerOnSelect && m_Controller != null)
                m_Controller.hideControllerModel = true;

            if (m_PlayHapticsOnSelectEntered && m_Controller != null)
                SendHapticImpulse(m_HapticSelectEnterIntensity, m_HapticSelectEnterDuration);

            if (m_PlayAudioClipOnSelectEntered && m_AudioClipForOnSelectEntered != null)
            {
                if (m_EffectsAudioSource == null)
                    CreateEffectsAudioSource();

                m_EffectsAudioSource.PlayOneShot(m_AudioClipForOnSelectEntered);
            }
        }

        /// <inheritdoc />
        protected internal override void OnSelectExiting(XRBaseInteractable interactable)
        {
            base.OnSelectExiting(interactable);

            // If another interactable takes this one, make sure toggle select state is set false
            m_ToggleSelectActive = false;
            m_WaitingForSecondDeactivate = false;

            if (m_Controller != null)
                m_Controller.hideControllerModel = false;

            if (m_PlayHapticsOnSelectExited && m_Controller != null)
                SendHapticImpulse(m_HapticSelectExitIntensity, m_HapticSelectExitDuration);

            if (m_PlayAudioClipOnSelectExited && m_AudioClipForOnSelectExited != null)
            {
                if (m_EffectsAudioSource == null)
                    CreateEffectsAudioSource();
                m_EffectsAudioSource.PlayOneShot(m_AudioClipForOnSelectExited);
            }
        }

        /// <inheritdoc />
        protected internal override void OnHoverEntering(XRBaseInteractable interactable)
        {
            base.OnHoverEntering(interactable);
            if (m_PlayHapticsOnHoverEntered && m_Controller != null)
                SendHapticImpulse(m_HapticHoverEnterIntensity, m_HapticHoverEnterDuration);

            if (m_PlayAudioClipOnHoverEntered && m_AudioClipForOnHoverEntered != null)
            {
                if (m_EffectsAudioSource == null)
                    CreateEffectsAudioSource();

                m_EffectsAudioSource.PlayOneShot(m_AudioClipForOnHoverEntered);
            }
        }

        /// <inheritdoc />
        protected internal override void OnHoverExiting(XRBaseInteractable interactable)
        {
            base.OnHoverExiting(interactable);
            if (m_PlayHapticsOnHoverExited && m_Controller != null)
                SendHapticImpulse(m_HapticHoverExitIntensity, m_HapticHoverExitDuration);

            if (m_PlayAudioClipOnHoverExited && m_AudioClipForOnHoverExited != null)
            {
                if (m_EffectsAudioSource == null)
                    CreateEffectsAudioSource();

                m_EffectsAudioSource.PlayOneShot(m_AudioClipForOnHoverExited);
            }
        }

        /// <summary>
        /// Play a haptic impulse on the controller if one is available.
        /// </summary>
        /// <param name="amplitude">Amplitude (from 0.0 to 1.0) to play impulse at.</param>
        /// <param name="duration">Duration (in seconds) to play haptic impulse.</param>
        /// <returns>Returns <see langword="true"/> if successful. Returns <see langword="false"/> otherwise.</returns>
        /// <seealso cref="XRBaseController.SendHapticImpulse"/>
        public bool SendHapticImpulse(float amplitude, float duration)
        {
            return m_Controller != null && m_Controller.SendHapticImpulse(amplitude, duration);
        }
    }
}
