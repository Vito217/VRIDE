using System;
using System.IO;
using NUnit.Framework;
using Unity.Android.Logcat;
using UnityEditor.Android;

internal class AndroidLogcatTestRuntime : AndroidLogcatRuntimeBase
{
    internal static readonly string kProjectSettingsPath = Path.Combine("Tests", "ProjectSettings", "AndroidLogcatSettings.asset");

    protected override string ProjectSettingsPath { get => kProjectSettingsPath; }

    public override IAndroidLogcatMessageProvider CreateMessageProvider(ADB adb, string filter, AndroidLogcat.Priority priority, int packageID, string logPrintFormat, string deviceId, Action<string> logCallbackAction)
    {
        return new AndroidLogcatFakeMessageProvider(adb, filter, priority, packageID, logPrintFormat, deviceId, logCallbackAction);
    }

    protected override AndroidTools CreateAndroidTools()
    {
        // Mac agents don't have SDK/NDK set up, for now return null for AndroidTools and make code work even if there's no Tools
        return null;
    }

    protected override AndroidLogcatDeviceQueryBase CreateDeviceQuery()
    {
        return new AndroidLogcatFakeDeviceQuery(this);
    }

    protected override AndroidLogcatSettings LoadEditorSettings()
    {
        return new AndroidLogcatSettings();
    }

    protected override void SaveEditorSettings(AndroidLogcatSettings settings)
    {
        // Don't save editor settings for tests
    }
}

internal class AndroidLogcatRuntimeTestBase
{
    protected AndroidLogcatTestRuntime m_Runtime;

    protected void Cleanup()
    {
        if (Directory.Exists("Tests"))
            Directory.Delete("Tests", true);
    }

    protected void InitRuntime(bool cleanup = true)
    {
        if (m_Runtime != null)
            throw new Exception("Runtime was not shutdown by previous test?");
        m_Runtime = new AndroidLogcatTestRuntime();
        if (cleanup)
            Cleanup();
        m_Runtime.Initialize();
    }

    protected void ShutdownRuntime(bool cleanup = true)
    {
        if (m_Runtime == null)
            throw new Exception("Runtime was not created?");
        m_Runtime.Shutdown();
        if (cleanup)
            Cleanup();
        m_Runtime = null;
    }
}
