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
    internal interface IAndroidLogcatMessageProvider
    {
        void Start();
        void Stop();
        void Kill();
        bool HasExited { get; }
    }

    internal class AndroidLogcatMessageProvider : IAndroidLogcatMessageProvider
    {
        private Process m_LogcatProcess;
        private ADB m_ADB;
        private string m_Filter;
        private AndroidLogcat.Priority m_Priority;
        private int m_PackageID;
        private string m_LogPrintFormat;
        private string m_DeviceId;
        private Action<string> m_LogCallbackAction;

        internal AndroidLogcatMessageProvider(ADB adb, string filter, AndroidLogcat.Priority priority, int packageID, string logPrintFormat, string deviceId, Action<string> logCallbackAction)
        {
            m_ADB = adb;
            m_Filter = filter;
            m_Priority = priority;
            m_PackageID = packageID;
            m_LogPrintFormat = logPrintFormat;
            m_DeviceId = deviceId;
            m_LogCallbackAction = logCallbackAction;
        }

        private string PriorityEnumToString(AndroidLogcat.Priority priority)
        {
            return priority.ToString().Substring(0, 1);
        }

        private string LogcatArguments()
        {
            var filterArg = string.Empty;
            if (!string.IsNullOrEmpty(m_Filter))
                filterArg = "--regex \"" + m_Filter + "\"";

            // Note: We're not using --regex argument, because some older Android device (prior to 7.0) doesn't support that
            var priority = PriorityEnumToString(m_Priority);
            if (m_PackageID > 0)
                return string.Format("-s {0} logcat --pid={1} -v {2} *:{3} {4}", m_DeviceId, m_PackageID, m_LogPrintFormat, priority, filterArg);

            return string.Format("-s {0} logcat -v {1} *:{2} {3}", m_DeviceId, m_LogPrintFormat, priority, filterArg);
        }

        public void Start()
        {
            var arguments = LogcatArguments();
            AndroidLogcatInternalLog.Log("\n\nStarting logcat\n\n");
            AndroidLogcatInternalLog.Log("{0} {1}", m_ADB.GetADBPath(), arguments);
            m_LogcatProcess = new Process();
            m_LogcatProcess.StartInfo.FileName = m_ADB.GetADBPath();
            m_LogcatProcess.StartInfo.Arguments = arguments;
            m_LogcatProcess.StartInfo.RedirectStandardError = true;
            m_LogcatProcess.StartInfo.RedirectStandardOutput = true;
            m_LogcatProcess.StartInfo.UseShellExecute = false;
            m_LogcatProcess.StartInfo.CreateNoWindow = true;
            m_LogcatProcess.OutputDataReceived += OutputDataReceived;
            m_LogcatProcess.ErrorDataReceived += OutputDataReceived;
            m_LogcatProcess.Start();

            m_LogcatProcess.BeginOutputReadLine();
            m_LogcatProcess.BeginErrorReadLine();
        }

        public void Stop()
        {
            if (m_LogcatProcess != null && !m_LogcatProcess.HasExited)
                m_LogcatProcess.Kill();

            m_LogcatProcess = null;
        }

        public void Kill()
        {
            // NOTE: DONT CALL CLOSE, or ADB process will stay alive all the time
            AndroidLogcatInternalLog.Log("Stopping logcat (process id {0})", m_LogcatProcess.Id);
            m_LogcatProcess.Kill();
        }

        public bool HasExited
        {
            get
            {
                return m_LogcatProcess.HasExited;
            }
        }

        private void OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            m_LogCallbackAction(e.Data);
        }
    }
}
#endif
