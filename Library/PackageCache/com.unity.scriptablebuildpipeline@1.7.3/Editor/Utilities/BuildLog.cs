
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using UnityEditor.Build.Pipeline.Interfaces;

namespace UnityEditor.Build.Pipeline.Utilities
{
    [Serializable]
    internal class BuildLog : IBuildLogger
    {
        [Serializable]
        internal struct LogEntry
        {
            public int ThreadId { get; set; }
            public double Time { get; set; }
            public LogLevel Level { get; set; }
            public string Message { get; set; }
        }

        [Serializable]
        internal class LogStep
        {
            List<LogStep> m_Children;
            List<LogEntry> m_Entries;

            public string Name { get; set; }
            public LogLevel Level { get; set; }
            public List<LogStep> Children { get { if (m_Children == null) m_Children = new List<LogStep>(); return m_Children; } }
            public List<LogEntry> Entries { get { if (m_Entries == null) m_Entries = new List<LogEntry>(); return m_Entries; } }
            public double DurationMS { get; private set; }
            public int ThreadId { get; set; }
            public double StartTime { get; set; }
            internal bool isThreaded;

            public bool HasChildren { get { return Children != null && Children.Count > 0; } }
            public bool HasEntries { get { return Entries != null && Entries.Count > 0; } }

            internal void Complete(double time)
            {
                DurationMS = time - StartTime;
            }
        }

        LogStep m_Root;
        [NonSerialized]
        Stack<LogStep> m_Stack;
        [NonSerialized]
        ThreadLocal<BuildLog> m_ThreadedLogs;
        [NonSerialized]
        Stopwatch m_WallTimer;

        public BuildLog()
        {
            m_WallTimer = Stopwatch.StartNew();
            m_Root = new LogStep();
            m_Stack = new Stack<LogStep>();
            m_Stack.Push(m_Root);
        }

        private BuildLog GetThreadSafeLog()
        {
            if (m_ThreadedLogs != null)
            {
                if (!m_ThreadedLogs.IsValueCreated)
                    m_ThreadedLogs.Value = new BuildLog();
                return m_ThreadedLogs.Value;
            }
            return this;
        }

        public void BeginBuildStep(LogLevel level, string stepName, bool multiThreaded)
        {
            BuildLog log = GetThreadSafeLog();
            BeginBuildStepInternal(log, level, stepName, multiThreaded);
        }

        private static void BeginBuildStepInternal(BuildLog log, LogLevel level, string stepName, bool multiThreaded)
        {
            LogStep node = new LogStep();
            node.Level = level;
            node.Name = stepName;
            node.StartTime = log.m_WallTimer.Elapsed.TotalMilliseconds;
            node.ThreadId = Thread.CurrentThread.ManagedThreadId;
            log.m_Stack.Peek().Children.Add(node);
            log.m_Stack.Push(node);
            if (multiThreaded)
            {
                Debug.Assert(log.m_ThreadedLogs == null);
                log.m_ThreadedLogs = new ThreadLocal<BuildLog>(true);
                log.m_ThreadedLogs.Value = log;
                node.isThreaded = true;
            }
        }

        public void EndBuildStep()
        {
            EndBuildStepInternal(GetThreadSafeLog());
        }

        private static void EndBuildStepInternal(BuildLog log)
        {
            Debug.Assert(log.m_Stack.Count > 1);
            LogStep node = log.m_Stack.Pop();
            node.Complete(log.m_WallTimer.Elapsed.TotalMilliseconds);
            
            if (node.isThreaded)
            {
                foreach (var subLog in log.m_ThreadedLogs.Values)
                {
                    if (subLog != log)
                    {
                        if (subLog.Root.HasChildren)
                        {
                            foreach(LogStep step in subLog.Root.Children)
                                step.StartTime += node.StartTime;
                            node.Children.AddRange(subLog.Root.Children);
                        }
                        if (subLog.Root.HasEntries)
                        {
                            for (int i = 0; i < subLog.Root.Entries.Count; i++)
                            {
                                LogEntry e = subLog.Root.Entries[i];
                                e.Time = e.Time + node.StartTime;
                                subLog.Root.Entries[i] = e;
                            }
                            node.Entries.AddRange(subLog.Root.Entries);
                        }
                    }
                }
                log.m_ThreadedLogs.Dispose();
                log.m_ThreadedLogs = null;
            }
        }

        internal LogStep Root { get { return m_Root; } }

        public void AddEntry(LogLevel level, string msg)
        {
            BuildLog log = GetThreadSafeLog();
            log.m_Stack.Peek().Entries.Add(new LogEntry() { Level = level, Message = msg, Time = log.m_WallTimer.Elapsed.TotalMilliseconds, ThreadId = Thread.CurrentThread.ManagedThreadId });
        }
    }

    internal static class BuildLogExtensions
    {
        static void AppendLineIndented(StringBuilder builder, int indentCount, string text)
        {
            for (int i = 0; i < indentCount; i++)
                builder.Append(" ");
            builder.AppendLine(text);
        }

        static void PrintNodeR(bool includeSelf, StringBuilder builder, int indentCount, BuildLog.LogStep node)
        {
            if (includeSelf)
                AppendLineIndented(builder, indentCount, $"[{node.Name}] {node.DurationMS * 1000}us");
            foreach (var msg in node.Entries)
            {
                string line = (msg.Level == LogLevel.Warning || msg.Level == LogLevel.Error) ? $"{msg.Level}: {msg.Message}" : msg.Message;
                AppendLineIndented(builder, indentCount + 1, line);
            }
            foreach (var child in node.Children)
                PrintNodeR(true, builder, indentCount + 1, child);
        }

        static public string FormatAsText(this BuildLog log)
        {
            StringBuilder builder = new StringBuilder();
            builder.AppendLine("Warning: The formatting in this file is subject to change.");
            PrintNodeR(false, builder, -1, log.Root);
            return builder.ToString();
        }

        static IEnumerable<string> IterateTEPLines(bool includeSelf, BuildLog.LogStep node)
        {
            ulong us = (ulong)(node.StartTime * 1000);
            if (includeSelf)
                yield return "{" + $"\"name\": \"{node.Name}\", \"ph\": \"X\", \"dur\": {node.DurationMS * 1000}, \"tid\": {node.ThreadId}, \"ts\": {us}, \"pid\": 1" + "}";

            foreach (var msg in node.Entries)
            {
                string line = (msg.Level == LogLevel.Warning || msg.Level == LogLevel.Error) ? $"{msg.Level}: {msg.Message}" : msg.Message;
                ulong us2 = (ulong)(node.StartTime * 1000);
                yield return "{" + $"\"name\": \"{line}\", \"ph\": \"i\", \"tid\": {msg.ThreadId}, \"ts\": {us2}, \"pid\": 1" + "}";
            }
            
            foreach (var child in node.Children)
                foreach (var r in IterateTEPLines(true, child))
                    yield return r;
        }

        static public string FormatAsTraceEventProfiler(this BuildLog log)
        {
            StringBuilder builder = new StringBuilder();
            builder.AppendLine("[");
            int i = 0;
            foreach (string line in IterateTEPLines(false, log.Root))
            {
                if (i != 0)
                    builder.Append(",");
                builder.AppendLine(line);
                i++;
            }
            builder.AppendLine("]");
            return builder.ToString();
        }
    }
}