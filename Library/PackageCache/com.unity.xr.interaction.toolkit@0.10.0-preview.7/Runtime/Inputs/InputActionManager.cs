using System;
using System.Collections.Generic;
using UnityEngine.InputSystem;

namespace UnityEngine.XR.Interaction.Toolkit.Inputs
{
    /// <summary>
    /// Use this class to automatically enable or disable all the inputs of type <see cref="InputAction"/>
    /// in a list of assets of type <see cref="InputActionAsset"/>.
    /// </summary>
    /// <remarks>
    /// Actions are initially disabled, meaning they do not listen/react to input yet. This class
    /// is used to mass enable actions so that they actively listen for input and run callbacks.
    /// </remarks>
    /// <seealso cref="InputAction"/>
    public class InputActionManager : MonoBehaviour
    {
        [SerializeField]
        [Tooltip("Input action assets to affect when inputs are enabled or disabled.")]
        List<InputActionAsset> m_ActionAssets;
        /// <summary>
        /// Input action assets to affect when inputs are enabled or disabled.
        /// </summary>
        public List<InputActionAsset> actionAssets
        {
            get => m_ActionAssets;
            set => m_ActionAssets = value ?? throw new ArgumentNullException(nameof(value));
        }

        protected void OnEnable()
        {
            EnableInput();
        }

        protected void OnDisable()
        {
            DisableInput();
        }

        /// <summary>
        /// Enable all actions referenced by this component.
        /// </summary>
        /// <remarks>
        /// This function will automatically be called when this <see cref="InputActionManager"/> component is enabled.
        /// However, this method can be called to enable input manually, such as after disabling it with <see cref="DisableInput"/>.
        /// <br />
        /// Note that enabling inputs will only enable the action maps contained within the referenced
        /// action map assets (see <see cref="actionAssets"/>).
        /// </remarks>
        /// <seealso cref="DisableInput"/>
        public void EnableInput()
        {
            if (m_ActionAssets == null)
                return;

            foreach (var actionAsset in m_ActionAssets)
            {
                if (actionAsset != null)
                {
                    actionAsset.Enable();
                }
            }
        }

        /// <summary>
        /// Disable all actions referenced by this component.
        /// </summary>
        /// <remarks>
        /// This function will automatically be called when this <see cref="InputActionManager"/> component is disabled.
        /// However, this method can be called to disable input manually, such as after enabling it with <see cref="EnableInput"/>.
        /// <br />
        /// Note that disabling inputs will only disable the action maps contained within the referenced
        /// action map assets (see <see cref="actionAssets"/>).
        /// </remarks>
        /// <seealso cref="EnableInput"/>
        public void DisableInput()
        {
            if (m_ActionAssets == null)
                return;

            foreach (var actionAsset in m_ActionAssets)
            {
                if (actionAsset != null)
                {
                    actionAsset.Disable();
                }
            }
        }
    }
}
