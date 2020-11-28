#if PLATFORM_ANDROID
using System;
using System.IO;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEditor.Android;
using System.Collections.Generic;
using System.Linq;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatUtilities
    {
        /// <summary>
        /// Capture the screenshot on the given device.
        /// </summary>
        /// <returns> Return the path to the screenshot on the PC. </returns>
        public static string CaptureScreen(ADB adb, string deviceId, out string error)
        {
            error = string.Empty;
            if (string.IsNullOrEmpty(deviceId))
            {
                error = "Invalid device id.";
                return null;
            }

            try
            {
                const string screenshotPathOnDevice = "/sdcard/screen.png";

                // Capture the screen on the device.
                var cmd = string.Format("-s {0} shell screencap {1}", deviceId, screenshotPathOnDevice);
                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);

                var errorMsg = "Unable to capture the screen for device ";
                var outputMsg = adb.Run(new[] { cmd }, errorMsg + deviceId);
                if (outputMsg.StartsWith(errorMsg))
                {
                    AndroidLogcatInternalLog.Log(outputMsg);
                    Debug.LogError(outputMsg);
                    error = outputMsg;
                    return null;
                }

                // Pull screenshot from the device to temp folder.
                var filePath = Path.Combine(Path.GetTempPath(), "screen_" + DateTime.Now.ToString("yyyy-MM-dd-HH-mm-ss") + ".png");
                cmd = string.Format("-s {0} pull {1} {2}", deviceId, screenshotPathOnDevice, filePath);
                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);

                errorMsg = "Unable to pull the screenshot from device ";
                outputMsg = adb.Run(new[] { cmd }, errorMsg + deviceId);
                if (outputMsg.StartsWith(errorMsg))
                {
                    AndroidLogcatInternalLog.Log(outputMsg);
                    Debug.LogError(outputMsg);
                    error = outputMsg;
                    return null;
                }

                return filePath;
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log("Exception caugth while capturing screen on device {0}. Details\r\n:{1}", deviceId, ex);
                error = ex.Message;
                return null;
            }
        }

        /// <summary>
        /// Get the top activity on the given device.
        /// </summary>
        public static bool GetTopActivityInfo(ADB adb, IAndroidLogcatDevice device, ref string packageName, ref int packagePid)
        {
            if (device == null)
                return false;
            try
            {
                var cmd = "-s " + device.Id + " shell \"dumpsys activity\" ";
                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);
                var output = adb.Run(new[] { cmd }, "Unable to get the top activity.");
                packagePid = AndroidLogcatUtilities.ParseTopActivityPackageInfo(output, out packageName);
                return packagePid != -1;
            }
            catch (Exception)
            {
                return false;
            }
        }

        /// <summary>
        /// Return the pid of the given package on the given device.
        /// </summary>
        public static int GetPidFromPackageName(ADB adb, IAndroidLogcatDevice device, string packageName)
        {
            if (device == null)
                return -1;

            try
            {
                string cmd = null;
                if (device.SupportsFilteringByPid)
                    cmd = string.Format("-s {0} shell pidof -s {1}", device.Id, packageName);
                else
                    cmd = string.Format("-s {0} shell ps", device.Id);

                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);
                var output = adb.Run(new[] { cmd }, "Unable to get the pid of the given packages.");
                if (string.IsNullOrEmpty(output))
                    return -1;

                if (device.SupportsFilteringByPid)
                {
                    AndroidLogcatInternalLog.Log(output);
                    return int.Parse(output);
                }

                return ParsePidInfo(packageName, output);
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log(ex.Message);
                return -1;
            }
        }

        public static string GetPackageNameFromPid(ADB adb, IAndroidLogcatDevice device, int processId)
        {
            if (device == null)
                return string.Empty;

            try
            {
                string cmd = string.Format("-s {0} shell ps -p {1} -o NAME", device.Id, processId);

                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);
                var output = adb.Run(new[] { cmd }, "Unable to get the package name for pid " + processId);
                if (string.IsNullOrEmpty(output))
                    return string.Empty;

                using (var sr = new StringReader(output))
                {
                    string line;
                    while ((line = sr.ReadLine().Trim()) != null)
                    {
                        if (line.Equals("NAME"))
                            continue;

                        return line;
                    }
                }

                AndroidLogcatInternalLog.Log("Unable to get the package name for pid " + processId + "\nOutput:\n" + output);
                return string.Empty;
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log(ex.Message);
                return string.Empty;
            }
        }

        /// <summary>
        /// Return the detail info of the given device.
        /// </summary>
        public static string RetrieveDeviceDetails(AndroidLogcatDevice device, string deviceId)
        {
            if (device == null)
                return deviceId;

            var manufacturer = device.Manufacturer;
            var model = device.Model;
            var release = device.OSVersion;
            var sdkVersion = device.APILevel;
            var abi = device.ABI;

            return string.Format("{0} {1} (version: {2}, abi: {3}, sdk: {4}, id: {5})", manufacturer, model, release, abi, sdkVersion, deviceId);
        }

        public static int ParsePidInfo(string packageName, string commandOutput)
        {
            string line = null;
            // Note: Regex is very slow, looping through string is much faster
            using (var sr = new StringReader(commandOutput))
            {
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.EndsWith(packageName))
                        break;
                }
            }

            if (string.IsNullOrEmpty(line))
            {
                AndroidLogcatInternalLog.Log("Cannot get process status for '{0}'.", packageName);
                return -1;
            }

            var regex = new Regex(@"\b\d+");
            Match match = regex.Match(line);
            if (!match.Success)
            {
                AndroidLogcatInternalLog.Log("Failed to parse pid of '{0}'from '{1}'.", packageName, line);
                return -1;
            }

            return int.Parse(match.Groups[0].Value);
        }

        public static int ParseTopActivityPackageInfo(string commandOutput, out string packageName)
        {
            packageName = "";
            if (string.IsNullOrEmpty(commandOutput))
                return -1;

            // Note: Regex is very slow, looping through string is much faster
            string line = null;
            using (var sr = new StringReader(commandOutput))
            {
                do
                {
                    while ((line = sr.ReadLine()) != null)
                    {
                        if (line.Contains("top-activity") ||        // Top Activity when device is not locked
                            line.Contains("top-sleeping"))          // Top Activity when device is locked
                            break;
                    }

                    if (string.IsNullOrEmpty(line))
                    {
                        AndroidLogcatInternalLog.Log("Cannot find top activity.");
                        return -1;
                    }

                    AndroidLogcatInternalLog.Log(line);

                    var reg = new Regex(@"(?<pid>\d+)\:(?<package>\S+)\/\S+\s+\(top-\S+\)");
                    var match = reg.Match(line);
                    if (!match.Success)
                    {
                        AndroidLogcatInternalLog.Log("Match '{0}' failed.", line);
                        return -1;
                    }

                    int pid = int.Parse(match.Groups["pid"].Value);

                    // There can be lines with (top-activity) at the end, but pid == 0, not sure what are those, but definetly not top activities
                    if (pid > 0)
                    {
                        packageName = match.Groups["package"].Value;
                        return pid;
                    }

                    // Continue looking for top activity
                }
                while (true);
            }
        }

        public static void OpenTerminal(string workingDirectory)
        {
            switch (Application.platform)
            {
                case RuntimePlatform.WindowsEditor:
                    System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo("cmd.exe") {WorkingDirectory = workingDirectory});
                    break;
                case RuntimePlatform.OSXEditor:
                    var pathsToCheck = new[]
                    {
                        "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal",
                        "/System/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal"
                    };
                    foreach (var p in pathsToCheck)
                    {
                        if (File.Exists(p))
                        {
                            System.Diagnostics.Process.Start(p, workingDirectory);
                            return;
                        }
                    }

                    throw new Exception(string.Format("Failed to launch Terminal app, tried following paths:\n{0}", string.Join("\n",  pathsToCheck)));
                default:
                    throw new Exception("Don't know how to open terminal on " + Application.platform.ToString());
            }
        }

        public static Version ParseVersionLegacy(string versionString)
        {
            int major = 0;
            int minor = 0;
            int build = 0;
            int revision = 0;
            var vals = versionString.Split('.');
            if (vals.Length > 0)
                int.TryParse(vals[0], out major);
            if (vals.Length > 1)
                int.TryParse(vals[1], out minor);
            if (vals.Length > 2)
                int.TryParse(vals[2], out build);
            if (vals.Length > 3)
                int.TryParse(vals[3], out revision);

            if (vals.Length <= 2)
                return new Version(major, minor);
            if (vals.Length <= 3)
                return new Version(major, minor, build);
            return new Version(major, minor, build, revision);
        }

        public static Version ParseVersion(string versionString)
        {
#if NET_2_0
            return ParseVersionLegacy(versionString);
#else
            var vals = versionString.Split('.');

            // Version.TryParse isn't capable of parsing digits without dots, for ex., 1
            if (vals.Length == 1)
            {
                int n;
                if (!int.TryParse(vals[0], out n))
                {
                    AndroidLogcatInternalLog.Log("Failed to parse android OS version '{0}'", versionString);
                    return new Version(0, 0);
                }
                return new Version(n, 0);
            }

            Version version;
            if (!Version.TryParse(versionString, out version))
            {
                AndroidLogcatInternalLog.Log("Failed to parse android OS version '{0}'", versionString);
                return new Version(0, 0);
            }
            return version;
#endif
        }

        public static BuildInfo ParseBuildInfo(string msg)
        {
            BuildInfo buildInfo;

            var reg = new Regex(@"Build type '(\S+)',\s+Scripting Backend '(\S+)',\s+CPU '(\S+)'");
            Match match = reg.Match(msg);

            buildInfo.buildType = match.Groups[1].Value.ToLower();
            buildInfo.scriptingImplementation = match.Groups[2].Value.ToLower();
            buildInfo.cpu = match.Groups[3].Value.ToLower();
            return buildInfo;
        }

        /// <summary>
        /// Returns symbol file by checking following extensions, for ex., if you're searching for libunity.so symbol file, it will first try to:
        /// - libunity.so
        /// - libunity.sym.so
        /// - libunity.dbg.so
        /// </summary>
        /// <param name="symbolPath"></param>
        /// <param name="libraryFile"></param>
        /// <returns></returns>
        internal static string GetSymbolFile(string symbolPath, string libraryFile)
        {
            var fullPath = Path.Combine(symbolPath, libraryFile);
            if (File.Exists(fullPath))
                return fullPath;

            var extensionsToTry = new[] { ".sym.so", ".dbg.so" };
            foreach (var e in extensionsToTry)
            {
                // Try sym.so extension
                fullPath = Path.Combine(symbolPath, Path.GetFileNameWithoutExtension(libraryFile) + e);
                if (File.Exists(fullPath))
                    return fullPath;
            }

            return string.Empty;
        }

        internal static string GetSymbolFile(IReadOnlyList<ReordableListItem> symbolPaths, string libraryFile)
        {
            foreach (var symbolPath in symbolPaths)
            {
                if (!symbolPath.Enabled)
                    continue;

                var file = GetSymbolFile(symbolPath.Name, libraryFile);
                if (!string.IsNullOrEmpty(file))
                    return file;
            }

            return string.Empty;
        }

        internal static bool ParseCrashLine(IReadOnlyList<ReordableListItem> regexs, string msg, out string address, out string libName)
        {
            foreach (var regexItem in regexs)
            {
                if (!regexItem.Enabled)
                    continue;

                var match = new Regex(regexItem.Name).Match(msg);
                if (match.Success)
                {
                    address = match.Groups["address"].Value;
                    libName = match.Groups["libName"].Value + ".so";
                    return true;
                }
            }

            address = null;
            libName = null;
            return false;
        }
    }
}
#else
namespace Unity.Android.Logcat
{
    internal class AndroidLogcatUtilities
    {
        public static void ShowActivePlatformNotAndroidMessage()
        {
            UnityEditor.EditorGUILayout.HelpBox("Please switch active platform to be Android in Build Settings Window.", UnityEditor.MessageType.Info);
        }
    }
}
#endif
