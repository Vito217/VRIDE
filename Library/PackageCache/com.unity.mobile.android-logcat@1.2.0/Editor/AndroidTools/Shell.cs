using System;
using System.Diagnostics;
using System.Text;

namespace Unity.Android.Logcat
{
    internal class ShellStartInfo
    {
        internal string FileName { set; get; }
        internal string Arguments { set; get; }
        internal string WorkingDirectory { set; get; }

        internal ShellStartInfo()
        {
            FileName = string.Empty;
            Arguments = string.Empty;
            WorkingDirectory = string.Empty;
        }
    }

    internal class ShellReturnInfo
    {
        private readonly ShellStartInfo m_StartInfo;
        private readonly int m_ExitCode;
        private readonly string m_StandardOut;
        private readonly string m_StandardErr;

        internal ShellStartInfo GetStartInfo()
        {
            return m_StartInfo;
        }

        internal int GetExitCode()
        {
            return m_ExitCode;
        }

        internal string GetStandardOut()
        {
            return m_StandardOut;
        }

        internal string GetStandardErr()
        {
            return m_StandardErr;
        }

        internal ShellReturnInfo(ShellStartInfo startInfo, int exitCode, string standardOut, string standardErr)
        {
            m_StartInfo = startInfo;
            m_ExitCode = exitCode;
            m_StandardOut = standardOut;
            m_StandardErr = standardErr;
        }
    }

    internal static class Shell
    {
        internal static ShellReturnInfo RunProcess(string fileName, string arguments)
        {
            return RunProcess(new ShellStartInfo() { FileName = fileName, Arguments = arguments });
        }

        internal static ShellReturnInfo RunProcess(string fileName, string arguments, string workingDirectory)
        {
            return RunProcess(new ShellStartInfo() { FileName = fileName, Arguments = arguments, WorkingDirectory = workingDirectory });
        }

        internal static ShellReturnInfo RunProcess(ShellStartInfo startInfo)
        {
            Process process = new Process();
            process.StartInfo.FileName = startInfo.FileName;
            process.StartInfo.Arguments = startInfo.Arguments;
            process.StartInfo.WorkingDirectory = startInfo.WorkingDirectory;
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.RedirectStandardOutput = true;
            process.StartInfo.RedirectStandardError = true;
            process.StartInfo.CreateNoWindow = true;
            var output = new StringBuilder();
            process.OutputDataReceived += new DataReceivedEventHandler((sender, e) =>
            {
                if (!string.IsNullOrEmpty(e.Data))
                {
                    output.AppendLine(e.Data);
                }
            });

            var error = new StringBuilder();
            process.ErrorDataReceived += new DataReceivedEventHandler((sender, e) =>
            {
                if (!string.IsNullOrEmpty(e.Data))
                {
                    error.AppendLine(e.Data);
                }
            });

            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();
            process.WaitForExit();
            var exitCode = process.ExitCode;
            process.Close();

            return new ShellReturnInfo(startInfo, exitCode, output.ToString(), error.ToString());
        }
    }
}
