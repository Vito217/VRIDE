#if PLATFORM_ANDROID
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEditor.Android;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatRetrieveDeviceIdsInput : IAndroidLogcatTaskInput
    {
        internal ADB adb;
        internal bool notifyListeners;
    }

    internal class AndroidLogcatRetrieveDeviceIdsResult : IAndroidLogcatTaskResult
    {
        internal struct DeviceInfo
        {
            internal string id;
            internal IAndroidLogcatDevice.DeviceState state;
        }

        internal List<DeviceInfo> deviceInfo = new List<DeviceInfo>();
        internal bool notifyListeners;
    }

    abstract class AndroidLogcatDeviceQueryBase
    {
        internal static Regex kDeviceInfoRegex = new Regex(@"(?<id>^\S+)\s+(?<state>\S+$)");

        protected IAndroidLogcatDevice m_SelectedDevice;
        protected Dictionary<string, IAndroidLogcatDevice> m_Devices = new Dictionary<string, IAndroidLogcatDevice>();
        protected AndroidLogcatRuntimeBase m_Runtime;

        internal event Action<IAndroidLogcatDevice> DeviceSelected;
        internal event Action DevicesUpdated;

        internal IAndroidLogcatDevice SelectedDevice
        {
            get
            {
                return m_SelectedDevice;
            }
        }

        internal IReadOnlyDictionary<string, IAndroidLogcatDevice> Devices
        {
            get
            {
                return m_Devices;
            }
        }

        internal IAndroidLogcatDevice FirstConnectedDevice
        {
            get
            {
                foreach (var d in m_Devices)
                {
                    if (d.Value.State != IAndroidLogcatDevice.DeviceState.Connected)
                        continue;
                    return d.Value;
                }
                return null;
            }
        }

        internal AndroidLogcatDeviceQueryBase(AndroidLogcatRuntimeBase runtime)
        {
            m_Runtime = runtime;
        }

        internal void Clear()
        {
            m_SelectedDevice = null;
        }

        internal void SelectDevice(IAndroidLogcatDevice device, bool notifyListeners = true)
        {
            if (m_SelectedDevice == device)
                return;

            if (device != null && device.State != IAndroidLogcatDevice.DeviceState.Connected)
            {
                AndroidLogcatInternalLog.Log("Trying to select device which is not connected: " + device.Id);
                if (m_SelectedDevice == null)
                    return;

                m_SelectedDevice = null;
            }
            else
            {
                m_SelectedDevice = device;
            }

            if (m_SelectedDevice != null && !m_Devices.Keys.Contains(m_SelectedDevice.Id))
                throw new Exception("Selected device is not among our listed devices");

            m_Runtime.ProjectSettings.LastSelectedDeviceId = m_SelectedDevice != null ? m_SelectedDevice.Id : "";

            if (notifyListeners)
                DeviceSelected?.Invoke(m_SelectedDevice);
        }

        internal static bool ParseDeviceInfo(string input, out string id, out IAndroidLogcatDevice.DeviceState state)
        {
            var result = kDeviceInfoRegex.Match(input);
            if (result.Success)
            {
                id = result.Groups["id"].Value;
                var stateValue = result.Groups["state"].Value.ToLowerInvariant();
                if (stateValue.Equals("device"))
                    state = IAndroidLogcatDevice.DeviceState.Connected;
                else if (stateValue.Equals("offline"))
                    state = IAndroidLogcatDevice.DeviceState.Disconnected;
                else if (stateValue.Equals("unauthorized"))
                    state = IAndroidLogcatDevice.DeviceState.Unauthorized;
                else
                    state = IAndroidLogcatDevice.DeviceState.Unknown;
                return true;
            }
            else
            {
                id = input;
                state = IAndroidLogcatDevice.DeviceState.Unknown;
                return false;
            }
        }

        internal IAndroidLogcatDevice GetDevice(string deviceId)
        {
            IAndroidLogcatDevice device;
            if (m_Devices.TryGetValue(deviceId, out device))
            {
                return device;
            }
            return null;
        }

        protected void IntegrateQueryDevices(IAndroidLogcatTaskResult resut)
        {
            var deviceIdsResult = ((AndroidLogcatRetrieveDeviceIdsResult)resut);
            var deviceInfos = deviceIdsResult.deviceInfo;

            foreach (var d in m_Devices)
            {
                d.Value.UpdateState(IAndroidLogcatDevice.DeviceState.Disconnected);
            }

            foreach (var info in deviceInfos)
            {
                GetOrCreateDevice(info.id).UpdateState(info.state);
            }

            // If our selected device was removed, deselect it
            if (m_SelectedDevice != null && m_SelectedDevice.State != IAndroidLogcatDevice.DeviceState.Connected)
            {
                m_SelectedDevice = null;
                if (deviceIdsResult.notifyListeners)
                    DeviceSelected?.Invoke(m_SelectedDevice);
            }

            if (m_SelectedDevice != null)
            {
                if (m_SelectedDevice != m_Devices[m_SelectedDevice.Id])
                    throw new Exception("The selected device is not among our list of devices");
            }

            DevicesUpdated?.Invoke();
        }

        private IAndroidLogcatDevice GetOrCreateDevice(string deviceId)
        {
            IAndroidLogcatDevice device;
            if (m_Devices.TryGetValue(deviceId, out device))
            {
                return device;
            }
            device = CreateDevice(deviceId);
            m_Devices[deviceId] = device;

            return device;
        }

        internal abstract void UpdateConnectedDevicesList(bool synchronous);

        protected abstract IAndroidLogcatDevice CreateDevice(string deviceId);
    }


    class AndroidLogcatDeviceQuery : AndroidLogcatDeviceQueryBase
    {
        protected const int kMillisecondsBetweenConsecutiveDeviceChecks = 1000;
        protected DateTime m_TimeOfLastDeviceListUpdate;

        internal AndroidLogcatDeviceQuery(AndroidLogcatRuntimeBase runtime)
            : base(runtime)
        {
            m_TimeOfLastDeviceListUpdate = DateTime.Now;
        }

        internal override void UpdateConnectedDevicesList(bool synchronous)
        {
            if ((DateTime.Now - m_TimeOfLastDeviceListUpdate).TotalMilliseconds < kMillisecondsBetweenConsecutiveDeviceChecks && !synchronous)
                return;
            m_TimeOfLastDeviceListUpdate = DateTime.Now;

            m_Runtime.Dispatcher.Schedule(new AndroidLogcatRetrieveDeviceIdsInput() { adb = m_Runtime.Tools.ADB, notifyListeners = true }, QueryDevicesAsync, IntegrateQueryDevices, synchronous);
        }

        private static IAndroidLogcatTaskResult QueryDevicesAsync(IAndroidLogcatTaskInput input)
        {
            var adb = ((AndroidLogcatRetrieveDeviceIdsInput)input).adb;

            if (adb == null)
                throw new NullReferenceException("ADB interface has to be valid");

            var result = new AndroidLogcatRetrieveDeviceIdsResult();
            result.notifyListeners = ((AndroidLogcatRetrieveDeviceIdsInput)input).notifyListeners;

            AndroidLogcatInternalLog.Log("{0} devices", adb.GetADBPath());
            try
            {
                var adbOutput = adb.Run(new[] { "devices" }, "Unable to list connected devices. ");
                foreach (var line in adbOutput.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).Select(line => line.Trim()))
                {
                    AndroidLogcatInternalLog.Log(" " + line);
                    AndroidLogcatRetrieveDeviceIdsResult.DeviceInfo info;
                    if (ParseDeviceInfo(line, out info.id, out info.state))
                        result.deviceInfo.Add(info);
                }
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log(ex.Message);
                result.deviceInfo = new List<AndroidLogcatRetrieveDeviceIdsResult.DeviceInfo>();
            }

            return result;
        }

        protected override IAndroidLogcatDevice CreateDevice(string deviceId)
        {
            return new AndroidLogcatDevice(m_Runtime.Tools.ADB, deviceId);
        }
    }
}
#endif
