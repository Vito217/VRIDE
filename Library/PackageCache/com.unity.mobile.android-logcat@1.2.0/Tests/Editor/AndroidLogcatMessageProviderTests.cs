using System;
using System.Collections;
using System.Diagnostics;
using NUnit.Framework;
using Unity.Android.Logcat;
using UnityEditor.Android;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEngine.TestTools;

internal class AndroidLogcatFakeMessageProvider : IAndroidLogcatMessageProvider
{
#pragma warning disable 0414
    private ADB m_ADB;
    private string m_Filter;
    private AndroidLogcat.Priority m_Priority;
    private int m_PackageID;
    private string m_LogPrintFormat;
    private string m_DeviceId;
    private Action<string> m_LogCallbackAction;
    private bool m_Started;

    private List<string> m_FakeMessages;
#pragma warning restore 0414
    internal AndroidLogcatFakeMessageProvider(ADB adb, string filter, AndroidLogcat.Priority priority, int packageID, string logPrintFormat, string deviceId, Action<string> logCallbackAction)
    {
        m_ADB = adb;
        m_Filter = filter;
        m_Priority = priority;
        m_PackageID = packageID;
        m_LogPrintFormat = logPrintFormat;
        m_DeviceId = deviceId;
        m_LogCallbackAction = logCallbackAction;

        m_FakeMessages = new List<string>();
        m_Started = false;
    }

    public void SupplyFakeMessage(string message)
    {
        m_FakeMessages.Add(message);
        if (m_Started)
            FlushFakeMessages();
    }

    private void FlushFakeMessages()
    {
        foreach (var m in m_FakeMessages)
        {
            m_LogCallbackAction(m);
        }
        m_FakeMessages.Clear();
    }

    public void Start()
    {
        m_Started = true;
        FlushFakeMessages();
    }

    public void Stop()
    {
        m_Started = false;
    }

    public void Kill()
    {
    }

    public bool HasExited
    {
        get
        {
            return false;
        }
    }
}

internal class AndroidLogcatMessagerProvideTests : AndroidLogcatRuntimeTestBase
{
    [Test]
    public void RegexFilterCorrectlyFormed()
    {
        var devices = new AndroidLogcatFakeDevice[] {new AndroidLogcatFakeDevice60("Fake60"), new AndroidLogcatFakeDevice90("Fake90")};
        var filter = ".*abc";
        InitRuntime();

        foreach (var device in devices)
        {
            foreach (var isRegexEnabled in new[] {true, false})
            {
                var logcat = new AndroidLogcat(m_Runtime, null, device, -1, AndroidLogcat.Priority.Verbose, ".*abc",
                    isRegexEnabled, new string[] {});
                var message = string.Format("Failure with {0} device, regex enabled: {1}", device.GetType().FullName,
                    isRegexEnabled.ToString());

                if (device.SupportsFilteringByRegex)
                {
                    if (isRegexEnabled)
                        Assert.IsTrue(logcat.Filter.Equals(filter), message);
                    else
                        Assert.IsTrue(logcat.Filter.Equals(Regex.Escape(filter)), message);
                }
                else
                {
                    Assert.IsTrue(logcat.Filter.Equals(filter), message);
                }
            }
        }

        ShutdownRuntime();
    }

    [Test]
    public void ManualRegexFilteringWorksAndroid60Devices()
    {
        var messages = new[]
        {
            @"10-25 14:27:56.862  2255  2255 I chromium: Help",
            @"10-25 14:27:56.863  2255  2255 I chromium: .abc"
        };

        InitRuntime();
        foreach (var regexIsEnabled in new[] {true, false})
        {
            foreach (var filter in new[] {"", ".abc", "...."})
            {
                var entries = new List<string>();
                var logcat = new AndroidLogcat(m_Runtime, null, new AndroidLogcatFakeDevice60("Fake60"), -1,
                    AndroidLogcat.Priority.Verbose, filter, regexIsEnabled, new string[] {});
                logcat.LogEntriesAdded += (List<AndroidLogcat.LogEntry> e) =>
                {
                    entries.AddRange(e.Select(m => m.message));
                };
                logcat.Start();

                var provider = (AndroidLogcatFakeMessageProvider)logcat.MessageProvider;
                foreach (var m in messages)
                    provider.SupplyFakeMessage(m);

                m_Runtime.OnUpdate();
                if (filter == "")
                {
                    Assert.IsTrue(entries.Contains(".abc"));
                    Assert.IsTrue(entries.Contains("Help"));
                }
                else if (filter == ".abc")
                {
                    Assert.IsTrue(entries.Contains(".abc"));
                    Assert.IsTrue(!entries.Contains("Help"));
                }
                else if (filter == "....")
                {
                    if (regexIsEnabled)
                    {
                        Assert.IsTrue(entries.Contains(".abc"));
                        Assert.IsTrue(entries.Contains("Help"));
                    }
                    else
                    {
                        Assert.IsFalse(entries.Contains(".abc"));
                        Assert.IsFalse(entries.Contains("Help"));
                    }
                }

                logcat.Stop();
            }
        }

        ShutdownRuntime();
    }

    [Test]
    public void ManualPidFilteringWorksAndroid60Devices()
    {
        var messages = new[]
        {
            @"10-25 14:27:56.862  1  2255 I chromium: Help",
            @"10-25 14:27:56.863  2  2255 I chromium: .abc"
        };

        InitRuntime();

        foreach (var pid in new[] { -1, 0, 1 })
        {
            var processIds = new List<int>();
            var logcat = new AndroidLogcat(m_Runtime, null, new AndroidLogcatFakeDevice60("Fake60"), pid, AndroidLogcat.Priority.Verbose, "", false, new string[] {});
            logcat.LogEntriesAdded += (List<AndroidLogcat.LogEntry> e) =>
            {
                processIds.AddRange(e.Select(m => m.processId));
            };
            logcat.Start();

            var provider = (AndroidLogcatFakeMessageProvider)logcat.MessageProvider;
            foreach (var m in messages)
                provider.SupplyFakeMessage(m);

            m_Runtime.OnUpdate();

            switch (pid)
            {
                // Should accept messages from any process id
                case -1:
                    Assert.IsTrue(processIds.Contains(1));
                    Assert.IsTrue(processIds.Contains(2));
                    break;
                // Should accept messages from any process id
                case 0:
                    Assert.IsTrue(processIds.Contains(1));
                    Assert.IsTrue(processIds.Contains(2));
                    break;
                // Should accept messages from process id which equals 1
                case 1:
                    Assert.IsTrue(processIds.Contains(1));
                    Assert.IsFalse(processIds.Contains(2));
                    break;
            }

            logcat.Stop();
        }


        ShutdownRuntime();
    }
}
