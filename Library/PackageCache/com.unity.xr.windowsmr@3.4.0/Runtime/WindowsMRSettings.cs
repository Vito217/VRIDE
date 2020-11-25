using System;
using System.Collections.Generic;

using UnityEngine;
using UnityEngine.XR.Management;

namespace UnityEngine.XR.WindowsMR
{
    /// <summary>Runtime settings for this XR Plugin.</summary>
    public class WindowsMRSettings : ScriptableObject
    {
        public enum DepthBufferOption
        {
            DepthBuffer16Bit,
            DepthBuffer24Bit
        }

        [SerializeField, Tooltip("Set the size of the depth buffer")]
        /// <summary>If using a shared depth buffer, this is the type of the depth buffer we should use.</summary>
        public DepthBufferOption DepthBufferFormat;

        [SerializeField, Tooltip("Enable depth buffer sharing")]
        /// <summary>True if we want to use a shared depth buffer, false otherwise.</summary>
        public bool UseSharedDepthBuffer;

#if !UNITY_EDITOR
        internal static WindowsMRSettings s_Settings;

        public void Awake()
        {
            s_Settings = this;
        }
#endif
    }
}
