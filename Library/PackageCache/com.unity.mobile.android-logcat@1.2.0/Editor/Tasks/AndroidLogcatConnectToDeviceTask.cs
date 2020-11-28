#if PLATFORM_ANDROID

using System;
using System.Collections.Generic;
using System.Linq;
using UnityEditor.Android;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatConnectToDeviceInput : IAndroidLogcatTaskInput
    {
        internal ADB adb;
        internal string ip;
        internal string port;
        internal string deviceId;
        internal bool setListeningPort;
    }

    internal class AndroidLogcatConnectToDeviceResult : IAndroidLogcatTaskResult
    {
        internal bool success;
        internal string message;
    }

    internal class AndroidLogcatConnectToDeviceTask
    {
        internal static IAndroidLogcatTaskResult Execute(IAndroidLogcatTaskInput input)
        {
            var result = new AndroidLogcatConnectToDeviceResult();

            try
            {
                var workInput = ((AndroidLogcatConnectToDeviceInput)input);
                var adb = workInput.adb;

                if (adb == null)
                    throw new NullReferenceException("ADB interface has to be valid");

                var ip = workInput.ip;
                var port = workInput.port;
                string cmd;
                if (workInput.setListeningPort)
                {
                    cmd = "-s " + workInput.deviceId + " tcpip " + port;
                    AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);
                    var resultTCPIP = adb.Run(new[] { cmd }, "Failed to adb tcpip " + port);
                    AndroidLogcatInternalLog.Log(resultTCPIP);
                    var wait = 3000;
                    AndroidLogcatInternalLog.Log("Waiting {0} ms until ADB returns", wait);
                    System.Threading.Thread.Sleep(wait);
                }

                cmd = "connect " + ip + ":" + port;
                AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);

                var errorMsg = "Unable to connect to ";
                var outputMsg = adb.Run(new[] { cmd }, errorMsg + ip + ":" + port);

                result.message = outputMsg;
                result.success = true;
                if (outputMsg.StartsWith(errorMsg) || outputMsg.StartsWith("failed to connect"))
                {
                    AndroidLogcatInternalLog.Log(outputMsg);
                    result.success = false;
                }
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log(ex.Message);
                result.success = false;
                result.message = ex.Message;
            }
            return result;
        }
    }
}
#endif
