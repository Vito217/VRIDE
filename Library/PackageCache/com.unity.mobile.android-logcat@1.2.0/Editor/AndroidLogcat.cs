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

[assembly: InternalsVisibleTo("Unity.Mobile.AndroidLogcat.EditorTests")]

namespace Unity.Android.Logcat
{
    internal class AndroidLogcat
    {
        public enum Priority
        {
            Verbose,
            Debug,
            Info,
            Warn,
            Error,
            Fatal
        }

        public struct LogEntry
        {
            public const string kTimeFormatWithYear = "yyyy/MM/dd HH:mm:ss.fff";
            public const string kTimeFormatWithoutYear = "MM/dd HH:mm:ss.fff";
            public static string s_TimeFormat = kTimeFormatWithYear;
            public LogEntry(string msg)
            {
                message =  msg;
                tag = string.Empty;
                dateTime = new DateTime();
                processId = -1;
                threadId = -1;
                priority = Priority.Info;
                this.message = this.message.TrimEnd(new[] { '\r', '\n' });
            }

            public LogEntry(LogEntry entry)
            {
                this.dateTime = entry.dateTime;
                this.processId = entry.processId;
                this.threadId = entry.threadId;
                this.priority = entry.priority;
                this.tag = entry.tag;
                this.message = entry.message;
            }

            public LogEntry(DateTime dateTime, int processId, int threadId, Priority priority, string tag, string message)
            {
                this.dateTime = dateTime;
                this.processId = processId;
                this.threadId = threadId;
                this.priority = priority;
                this.tag = tag ?? string.Empty;
                this.message = message ?? string.Empty;
                this.message = this.message.TrimEnd(new[] { '\r', '\n' });
            }

            public DateTime dateTime;
            public int processId;
            public int threadId;
            public Priority priority;
            public string tag;
            public string message;

            public override string ToString()
            {
                return string.Format("{0} {1} {2} {3} {4}: {5}", dateTime.ToString(s_TimeFormat), processId, threadId, priority, tag, message);
            }

            public static void SetTimeFormat(string timeFormat)
            {
                s_TimeFormat = timeFormat;
            }
        }

        private AndroidLogcatRuntimeBase m_Runtime;
        private ADB adb;

        private readonly IAndroidLogcatDevice m_Device;
        private readonly int m_PackagePid;
        private readonly Priority m_MessagePriority;
        private string m_Filter;
        private readonly bool m_FilterIsRegex;
        private Regex m_ManualFilterRegex;
        private readonly string[] m_Tags;

        public IAndroidLogcatDevice Device { get { return m_Device; } }

        public int PackagePid { get { return m_PackagePid; } }

        public Priority MessagePriority { get { return m_MessagePriority; } }

        public string Filter { get { return m_Filter; }  set { m_Filter = value; } }

        public string[] Tags { get { return m_Tags; } }

        public event Action<List<LogEntry>> LogEntriesAdded;

        public event Action<IAndroidLogcatDevice> Disconnected;

        public event Action<IAndroidLogcatDevice> Connected;

        private IAndroidLogcatMessageProvider m_MessageProvider;

        private List<string> m_CachedLogLines = new List<string>();

        public bool IsConnected
        {
            get
            {
                if (m_MessageProvider == null)
                    return false;
                try
                {
                    return !m_MessageProvider.HasExited;
                }
                catch (Exception ex)
                {
                    UnityEngine.Debug.LogError(ex.Message);
                    return false;
                }
            }
        }

        public IAndroidLogcatMessageProvider MessageProvider
        {
            get { return m_MessageProvider; }
        }

        public AndroidLogcat(AndroidLogcatRuntimeBase runtime, ADB adb, IAndroidLogcatDevice device, int packagePid, Priority priority, string filter, bool filterIsRegex, string[] tags)
        {
            this.m_Runtime = runtime;
            this.adb = adb;
            this.m_Device = device;
            this.m_PackagePid = packagePid;
            this.m_MessagePriority = priority;
            this.m_FilterIsRegex = filterIsRegex;
            InitFilterRegex(filter);
            this.m_Tags = tags;

            LogEntry.SetTimeFormat(m_Device.SupportYearFormat ? LogEntry.kTimeFormatWithYear : LogEntry.kTimeFormatWithoutYear);
        }

        private void InitFilterRegex(string filter)
        {
            if (string.IsNullOrEmpty(filter))
                return;

            if (m_Device.SupportsFilteringByRegex)
            {
                // When doing searching by filter, we use --regex command.
                // Note: there's no command line argument to disable or enable regular expressions for logcat
                // Thus when we want to disable regular expressions, we simply provide filter with escaped characters to --regex command
                this.m_Filter = m_FilterIsRegex ? filter : Regex.Escape(filter);
                return;
            }

            if (!this.m_FilterIsRegex)
            {
                this.m_Filter = filter;
                return;
            }

            try
            {
                this.m_Filter = filter;
                m_ManualFilterRegex = new Regex(m_Filter, RegexOptions.Compiled);
            }
            catch (Exception ex)
            {
                var error = string.Format("Input search filter '{0}' is not a valid regular expression.", Regex.Escape(m_Filter));
                AndroidLogcatInternalLog.Log(error);

                throw new ArgumentException(error, ex);
            }
        }

        internal void Start()
        {
            // For logcat arguments and more details check https://developer.android.com/studio/command-line/logcat
            m_Runtime.Update += OnUpdate;

            m_MessageProvider = m_Runtime.CreateMessageProvider(adb, Filter, MessagePriority, m_Device.SupportsFilteringByPid ? PackagePid : 0, LogPrintFormat, m_Device != null ? m_Device.Id : null, OnDataReceived);
            m_MessageProvider.Start();

            Connected?.Invoke(Device);
        }

        internal void Stop()
        {
            m_CachedLogLines.Clear();
            m_BuildInfos.Clear();
            m_Runtime.Update -= OnUpdate;
            if (m_MessageProvider != null && !m_MessageProvider.HasExited)
            {
                // NOTE: DONT CALL CLOSE, or ADB process will stay alive all the time
                m_MessageProvider.Kill();
            }

            m_MessageProvider = null;
        }

        internal void Clear()
        {
            if (m_MessageProvider != null)
                throw new InvalidOperationException("Cannot clear logcat when logcat process is alive.");

            AndroidLogcatInternalLog.Log("{0} -s {1} logcat -c", adb.GetADBPath(), Device.Id);
            var adbOutput = adb.Run(new[] { "-s", Device.Id, "logcat", "-c" }, "Failed to clear logcat.");
            AndroidLogcatInternalLog.Log(adbOutput);
        }

        void OnUpdate()
        {
            if (m_MessageProvider == null)
                return;

            if (m_MessageProvider.HasExited)
            {
                Stop();

                Disconnected?.Invoke(Device);

                return;
            }

            List<LogEntry> entries = new List<LogEntry>();
            lock (m_CachedLogLines)
            {
                if (m_CachedLogLines.Count == 0)
                    return;

                var needFilterByPid = !m_Device.SupportsFilteringByPid && PackagePid > 0;
                var needFilterByTags = Tags != null && Tags.Length > 0;
                var needFilterBySearch = !m_Device.SupportsFilteringByRegex  && !string.IsNullOrEmpty(Filter);
                Regex regex = LogParseRegex;
                foreach (var logLine in m_CachedLogLines)
                {
                    var m = regex.Match(logLine);
                    if (!m.Success)
                    {
                        // The reason we need to check `needFilterByTags` is we don't really want to show the error logs that we can't parse if a tag is chosen.
                        // For logs we can't parse, please refer to https://gitlab.cds.internal.unity3d.com/upm-packages/mobile/mobile-android-logcat/issues/44
                        // And we should remove this check once #44 is fixed completely.
                        if (!needFilterByTags)
                            entries.Add(LogEntryParserErrorFor(logLine));
                        continue;
                    }

                    if (needFilterByPid && Int32.Parse(m.Groups["pid"].Value) != PackagePid)
                        continue;

                    if (needFilterByTags && !MatchTagsFilter(m.Groups["tag"].Value))
                        continue;

                    if (needFilterBySearch && !MatchSearchFilter(m.Groups["msg"].Value))
                        continue;

                    entries.Add(ParseLogEntry(m));
                }
                m_CachedLogLines.Clear();
            }

            if (entries.Count == 0)
                return;

            ResolveStackTrace(entries);
            LogEntriesAdded(entries);
        }

        private LogEntry LogEntryParserErrorFor(string msg)
        {
            return new LogEntry(msg);
        }

        private bool MatchTagsFilter(string tagInMsg)
        {
            foreach (var tag in Tags)
            {
                if (tagInMsg.Contains(tag))
                    return true;
            }

            return false;
        }

        private bool MatchSearchFilter(string msg)
        {
            return m_FilterIsRegex ? m_ManualFilterRegex.Match(msg).Success : msg.Contains(Filter);
        }

        private LogEntry ParseLogEntry(Match m)
        {
            DateTime dateTime;
            var dateValue = m.Groups["date"].Value;
            if (LogPrintFormat == kThreadTime)
                dateValue = "1999-" + dateValue;

            try
            {
                dateTime = DateTime.Parse(dateValue);
            }
            catch (Exception ex)
            {
                dateTime = new DateTime();
                AndroidLogcatInternalLog.Log("Failed to parse date: " + dateValue + "\n" + ex.Message);
            }

            var entry = new LogEntry(
                dateTime,
                Int32.Parse(m.Groups["pid"].Value),
                Int32.Parse(m.Groups["tid"].Value),
                PriorityStringToEnum(m.Groups["priority"].Value),
                m.Groups["tag"].Value,
                m.Groups["msg"].Value);

            if ((entry.priority == Priority.Info && entry.tag.GetHashCode() == kUnityHashCode && entry.message.StartsWith("Built from")) ||
                (entry.priority == Priority.Error && entry.tag.GetHashCode() == kCrashHashCode && entry.message.StartsWith("Build type")))
            {
                m_BuildInfos[entry.processId] = AndroidLogcatUtilities.ParseBuildInfo(entry.message);
            }

            if (entry.priority == Priority.Fatal && entry.tag.GetHashCode() == kDebugHashCode && entry.message.StartsWith("pid:"))
            {
                // Crash reported by Android for some pid, need to update buildInfo information for this new pid as well
                ParseCrashBuildInfo(entry.processId, entry.message);
            }

            return entry;
        }

        private Priority PriorityStringToEnum(string priority)
        {
            switch (priority)
            {
                case "V": return Priority.Verbose;
                case "D": return Priority.Debug;
                case "I": return Priority.Info;
                case "W": return Priority.Warn;
                case "E": return Priority.Error;
                case "F": return Priority.Fatal;

                default:
                    throw new InvalidOperationException(string.Format("Invalid `priority` ({0}) in log entry.", priority));
            }
        }

        private void ParseCrashBuildInfo(int processId, string msg)
        {
            var reg = new Regex(@"pid: '(.+)'");
            Match match = reg.Match(msg);

            if (match.Success)
            {
                int pid = Int32.Parse(match.Groups[1].Value);
                if (pid != processId && m_BuildInfos.ContainsKey(pid))
                    m_BuildInfos[processId] = m_BuildInfos[pid];
            }
        }

        public struct UnresolvedAddress
        {
            public int logEntryIndex;
            public string unresolvedAddress;
        };

        private void ResolveStackTrace(List<LogEntry> entries)
        {
            var unresolvedAddresses = new Dictionary<KeyValuePair<BuildInfo, string>, List<UnresolvedAddress>>();

            // Gather unresolved address if there are any
            for (int i = 0; i < entries.Count; i++)
            {
                var entry = entries[i];
                // Only process stacktraces from Error/Fatal priorities
                if (entry.priority != Priority.Error && entry.priority != Priority.Fatal)
                    continue;

                // Only process stacktraces if tag is "CRASH" or "DEBUG"
                if (entry.tag.GetHashCode() != kCrashHashCode && entry.tag.GetHashCode() != kDebugHashCode)
                    continue;

                BuildInfo buildInfo;
                // Unknown build info, that means we don't know where the symbols are located
                if (!m_BuildInfos.TryGetValue(entry.processId, out buildInfo))
                    continue;

                string address, libName;
                if (!AndroidLogcatUtilities.ParseCrashLine(m_Runtime.Settings.StacktraceResolveRegex, entry.message, out address, out libName))
                    continue;

                List<UnresolvedAddress> addresses;
                var key = new KeyValuePair<BuildInfo, string>(buildInfo, libName);
                if (!unresolvedAddresses.TryGetValue(key, out addresses))
                    unresolvedAddresses[key] = new List<UnresolvedAddress>();

                unresolvedAddresses[key].Add(new UnresolvedAddress() { logEntryIndex = i, unresolvedAddress = address });
            }

            var engineDirectory = BuildPipeline.GetPlaybackEngineDirectory(BuildTarget.Android, BuildOptions.None);


            // Resolve addresses
            foreach (var u in unresolvedAddresses)
            {
                var buildInfo = u.Key.Key;
                var libName = u.Key.Value;

                var addresses = u.Value;
                var symbolPath = CombinePaths(engineDirectory, "Variations", buildInfo.scriptingImplementation, buildInfo.buildType, "Symbols", buildInfo.cpu);
                var libpath = AndroidLogcatUtilities.GetSymbolFile(symbolPath, libName);

                // For optimizations purposes, we batch addresses which belong to same library, so addr2line can be ran less
                try
                {
                    string[] result;
                    if (!string.IsNullOrEmpty(libpath))
                        result = m_Runtime.Tools.RunAddr2Line(libpath, addresses.Select(m => m.unresolvedAddress).ToArray());
                    else
                    {
                        result = new string[addresses.Count];
                        for (int i = 0; i < addresses.Count; i++)
                            result[i] = string.Empty;
                    }

                    for (int i = 0; i < addresses.Count; i++)
                    {
                        var idx = addresses[i].logEntryIndex;
                        var append = string.IsNullOrEmpty(result[i]) ? "(Not Resolved)" : result[i];
                        entries[idx] = new LogEntry(entries[idx]) { message = ModifyLogEntry(entries[idx].message, append, false)};
                    }
                }
                catch (Exception ex)
                {
                    for (int i = 0; i < addresses.Count; i++)
                    {
                        var idx = addresses[i].logEntryIndex;
                        entries[idx] = new LogEntry(entries[idx]) { message = ModifyLogEntry(entries[idx].message, "(Addr2Line failure)", true) };
                        var errorMessage = new StringBuilder();
                        errorMessage.AppendLine("Addr2Line failure");
                        errorMessage.AppendLine("Full Entry Message: " + entries[idx].message);
                        errorMessage.AppendLine("Scripting Backend: " + buildInfo.scriptingImplementation);
                        errorMessage.AppendLine("Build Type: " + buildInfo.buildType);
                        errorMessage.AppendLine("CPU: " + buildInfo.cpu);
                        errorMessage.AppendLine(ex.Message);
                        UnityEngine.Debug.LogError(errorMessage.ToString());
                    }
                }
            }
        }

        private string CombinePaths(params string[] paths)
        {
            // Unity hasn't implemented System.IO.Path(string[]), we have to do it on our own.
            if (paths.Length == 0)
                return "";

            string path = paths[0];
            for (int i = 1; i < paths.Length; ++i)
                path = System.IO.Path.Combine(path, paths[i]);
            return path;
        }

        private bool ParseCrashMessage(string msg, out string address, out string libName)
        {
            var match = m_CrashMessageRegex.Match(msg);
            if (match.Success)
            {
                address = match.Groups[1].Value;
                libName = match.Groups[2].Value;
                return true;
            }
            address = null;
            libName = null;
            return false;
        }

        private string ModifyLogEntry(string msg, string appendText, bool keeplOriginalMessage)
        {
            if (keeplOriginalMessage)
            {
                return msg + " " + appendText;
            }
            else
            {
                var match = m_CrashMessageRegex.Match(msg);
                return match.Success ? match.Groups[0].Value + " " + appendText : msg + " " + appendText;
            }
        }

        private string PriorityEnumToString(Priority priority)
        {
            return priority.ToString().Substring(0, 1);
        }

        private void OnDataReceived(string message)
        {
            // You can receive null string, when you put out USB cable out of PC and logcat connection is lost
            if (message == null)
                return;

            lock (m_CachedLogLines)
            {
                m_CachedLogLines.Add(message);
            }
        }

        internal Regex LogParseRegex
        {
            get { return m_Device.SupportYearFormat  ? m_LogCatEntryYearRegex : m_LogCatEntryThreadTimeRegex; }
        }

        /// <summary>
        /// Returns log print format used with adb logcat -v LogPrintFormat
        /// Note: Old android devices don't support all -v formats
        /// For ex., on Android 5.1.1 only these -v are available [brief process tag thread raw time threadtime long]
        /// While on Android 7.0, -v can have [brief color epoch long monotonic printable process raw tag thread threadtime time uid usec UTC year zone]
        /// </summary>
        internal string LogPrintFormat
        {
            get { return m_Device.SupportYearFormat ? kYearTime : kThreadTime; }
        }

        private Dictionary<int, BuildInfo> m_BuildInfos = new Dictionary<int, BuildInfo>();

        internal static Regex m_CrashMessageRegex = new Regex(@"^\s*#\d{2}\s*pc\s([a-fA-F0-9]{8}).*(libunity\.so|libmain\.so)", RegexOptions.Compiled);
        // Regex for messages produced via 'adb logcat -s -v year *:V'
        internal static Regex m_LogCatEntryYearRegex = new Regex(@"(?<date>\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}\.\d{3})\s+(?<pid>\d+)\s+(?<tid>\d+)\s+(?<priority>[VDIWEFS])\s+(?<tag>.+?)\s*:\s(?<msg>.*)", RegexOptions.Compiled);

        // Regex for messages produced via 'adb logcat -s -v threadtime *:V'
        internal static Regex m_LogCatEntryThreadTimeRegex = new Regex(@"(?<date>\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}\.\d{3})\s+(?<pid>\d+)\s+(?<tid>\d+)\s+(?<priority>[VDIWEFS])\s+(?<tag>.+?)\s*:\s(?<msg>.*)", RegexOptions.Compiled);


        internal static readonly int kUnityHashCode = "Unity".GetHashCode();
        internal static readonly int kCrashHashCode = "CRASH".GetHashCode();
        internal static readonly int kDebugHashCode = "DEBUG".GetHashCode();

        // Log PrintFormats
        internal const string kThreadTime = "threadtime";
        internal const string kYearTime = "year";
    }
}
#endif
