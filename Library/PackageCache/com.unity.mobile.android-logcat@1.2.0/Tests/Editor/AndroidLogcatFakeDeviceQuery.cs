using System;
using System.Collections.Generic;
using System.Linq;
using Unity.Android.Logcat;

class AndroidLogcatFakeDeviceQuery : AndroidLogcatDeviceQueryBase
{
    List<string> m_QueuedInfos = new List<string>();

    internal AndroidLogcatFakeDeviceQuery(AndroidLogcatRuntimeBase runtime)
        : base(runtime)
    {
    }

    internal void QueueDeviceInfos(string infos)
    {
        m_QueuedInfos.Add(infos);
    }

    internal override void UpdateConnectedDevicesList(bool synchronous)
    {
        m_Runtime.Dispatcher.Schedule(new AndroidLogcatRetrieveDeviceIdsInput() { adb = null, notifyListeners = true }, QueryDevicesAsync, IntegrateQueryDevices, synchronous);
    }

    private IAndroidLogcatTaskResult QueryDevicesAsync(IAndroidLogcatTaskInput input)
    {
        var result = new AndroidLogcatRetrieveDeviceIdsResult();
        result.notifyListeners = ((AndroidLogcatRetrieveDeviceIdsInput)input).notifyListeners;

        try
        {
            var adbOutput = m_QueuedInfos.Count > 0 ? m_QueuedInfos[0] : string.Empty;
            if (m_QueuedInfos.Count > 0)
                m_QueuedInfos.RemoveAt(0);

            foreach (var line in adbOutput.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).Select(line => line.Trim()))
            {
                AndroidLogcatRetrieveDeviceIdsResult.DeviceInfo info;
                if (ParseDeviceInfo(line, out info.id, out info.state))
                    result.deviceInfo.Add(info);
            }
        }
        catch (Exception)
        {
            result.deviceInfo = new List<AndroidLogcatRetrieveDeviceIdsResult.DeviceInfo>();
        }

        return result;
    }

    protected override IAndroidLogcatDevice CreateDevice(string deviceId)
    {
        return new AndroidLogcatFakeDevice90(deviceId);
    }
}
