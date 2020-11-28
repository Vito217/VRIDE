#if PLATFORM_ANDROID
using System.Collections.Generic;
using System.Diagnostics;
using System;
using System.Text.RegularExpressions;
using System.Linq;
using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEditor.Android;
using System.IO;


namespace Unity.Android.Logcat
{
    internal abstract class AndroidLogcatRuntimeBase
    {
        protected AndroidLogcatDispatcher m_Dispatcher;
        protected AndroidLogcatSettings m_Settings;
        protected AndroidLogcatProjectSettings m_ProjectSettings;
        protected AndroidTools m_Tools;
        protected AndroidLogcatDeviceQueryBase m_DeviceQuery;
        protected bool m_Initialized;

        protected abstract string ProjectSettingsPath { get; }

        private void ValidateIsInitialized()
        {
            if (!m_Initialized)
                throw new Exception("Runtime is not initialized");
        }

        public AndroidLogcatDispatcher Dispatcher
        {
            get { ValidateIsInitialized(); return m_Dispatcher; }
        }

        public AndroidLogcatSettings Settings
        {
            get { ValidateIsInitialized(); return m_Settings; }
        }

        public AndroidLogcatProjectSettings ProjectSettings
        {
            get { ValidateIsInitialized(); return m_ProjectSettings; }
        }

        public AndroidTools Tools
        {
            get { ValidateIsInitialized(); return m_Tools; }
        }

        public AndroidLogcatDeviceQueryBase DeviceQuery
        {
            get { ValidateIsInitialized(); return m_DeviceQuery; }
        }

        public abstract IAndroidLogcatMessageProvider CreateMessageProvider(ADB adb, string filter, AndroidLogcat.Priority priority, int packageID, string logPrintFormat, string deviceId, Action<string> logCallbackAction);
        protected abstract AndroidLogcatDeviceQueryBase CreateDeviceQuery();
        protected abstract AndroidLogcatSettings LoadEditorSettings();
        protected abstract AndroidTools CreateAndroidTools();
        protected abstract void SaveEditorSettings(AndroidLogcatSettings settings);

        public virtual void Initialize()
        {
            m_Dispatcher = new AndroidLogcatDispatcher(this);
            m_Dispatcher.Initialize();

            m_Settings = LoadEditorSettings();

            Directory.CreateDirectory(Path.GetDirectoryName(ProjectSettingsPath));
            m_ProjectSettings = AndroidLogcatProjectSettings.Load(ProjectSettingsPath);
            if (m_ProjectSettings == null)
            {
                m_ProjectSettings = new AndroidLogcatProjectSettings();
                m_ProjectSettings.Reset();
            }

            m_Tools = CreateAndroidTools();
            m_DeviceQuery = CreateDeviceQuery();

            m_Initialized = true;
        }

        public virtual void Shutdown()
        {
            Closing?.Invoke();
            // ProjectSettings is accessing some information from runtime during save
            AndroidLogcatProjectSettings.Save(m_ProjectSettings, ProjectSettingsPath, this);
            SaveEditorSettings(m_Settings);

            m_Initialized = false;
            m_Settings = null;
            m_ProjectSettings = null;
            m_Tools = null;
            m_Dispatcher.Shutdown();
            m_Dispatcher = null;
        }

        public void OnUpdate()
        {
            Update?.Invoke();
        }

        public event Action Update;
        public event Action Closing;
    }

    internal class AndroidLogcatRuntime : AndroidLogcatRuntimeBase
    {
        private static readonly string kProjectSettingsPath = Path.Combine("ProjectSettings", "AndroidLogcatSettings.asset");

        protected override string ProjectSettingsPath { get => kProjectSettingsPath; }

        public override IAndroidLogcatMessageProvider CreateMessageProvider(ADB adb, string filter, AndroidLogcat.Priority priority, int packageID, string logPrintFormat, string deviceId,
            Action<string> logCallbackAction)
        {
            return new AndroidLogcatMessageProvider(adb, filter, priority, packageID, logPrintFormat, deviceId, logCallbackAction);
        }

        public override void Initialize()
        {
            EditorApplication.update += OnUpdate;
            base.Initialize();
        }

        public override void Shutdown()
        {
            base.Shutdown();
            EditorApplication.update -= OnUpdate;
        }

        protected override AndroidLogcatDeviceQueryBase CreateDeviceQuery()
        {
            return new AndroidLogcatDeviceQuery(this);
        }

        protected override AndroidTools CreateAndroidTools()
        {
            return new AndroidTools();
        }

        protected override AndroidLogcatSettings LoadEditorSettings()
        {
            return AndroidLogcatSettings.Load();
        }

        protected override void SaveEditorSettings(AndroidLogcatSettings settings)
        {
            AndroidLogcatSettings.Save(settings);
        }
    }
}
#endif
