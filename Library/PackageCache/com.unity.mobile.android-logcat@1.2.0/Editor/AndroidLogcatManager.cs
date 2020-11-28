#if PLATFORM_ANDROID
using System.Collections.Generic;
using System.Diagnostics;
using System;
using System.Text.RegularExpressions;
using System.Linq;
using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEditor.Android;
using System.Text;


namespace Unity.Android.Logcat
{
    internal class AndroidLogcatManager : ScriptableSingleton<AndroidLogcatManager>
    {
        private AndroidLogcatRuntimeBase m_Runtime;

        internal void OnEnable()
        {
            Initialize();
        }

        internal void OnDisable()
        {
            if (m_Runtime != null)
            {
                m_Runtime.Shutdown();
                m_Runtime = null;
            }
        }

        private void Initialize()
        {
            if (m_Runtime != null)
                return;

            m_Runtime = new AndroidLogcatRuntime();
            m_Runtime.Initialize();
        }

        internal AndroidLogcatRuntimeBase Runtime
        {
            get
            {
                Initialize();
                return m_Runtime;
            }
        }
    }
}
#endif
