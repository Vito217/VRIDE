using System;

namespace UnityEditor.Build.Pipeline.Interfaces
{
    internal enum LogLevel
    {
        Error,
        Warning,
        Info,
        Verbose
    }

    internal interface IBuildLogger : IContextObject
    {
        void AddEntry(LogLevel level, string msg);
        
        void BeginBuildStep(LogLevel level, string stepName, bool subStepsCanBeThreaded);
        void EndBuildStep();
    }

    internal struct ScopedBuildStep : IDisposable
    {
        IBuildLogger m_Logger;
        public ScopedBuildStep(LogLevel level, string stepName, IBuildLogger logger, bool multiThreaded)
        {
            m_Logger = logger;
            m_Logger?.BeginBuildStep(level, stepName, multiThreaded);
        }

        public void Dispose()
        {
            m_Logger?.EndBuildStep();
        }
    }

    static internal class BuildLogUtil
    {
        public static void AddEntrySafe(this IBuildLogger log, LogLevel level, string msg)
        {
            if (log != null)
            {
                log.AddEntry(level, msg);
            }
        }

        public static ScopedBuildStep ScopedStep(this IBuildLogger log, LogLevel level, string stepName, bool multiThreaded=false)
        {
            return new ScopedBuildStep(level, stepName, log, multiThreaded);
        }
    }
 
}