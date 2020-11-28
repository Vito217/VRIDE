using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using Unity.Android.Logcat;
using UnityEditor.Android;


class AndroidLogcatDeviceQueryTests : AndroidLogcatRuntimeTestBase
{
    int m_UpdatedCount = 0;
    int m_SelectedCount = 0;

    [Test]
    public void CheckConnections()
    {
        m_UpdatedCount = 0;
        m_SelectedCount = 0;
        bool notUsed = true;
        InitRuntime();

        var query = (AndroidLogcatFakeDeviceQuery)m_Runtime.DeviceQuery;
        query.DeviceSelected += Query_DeviceSelected;
        query.DevicesUpdated += Query_DevicesUpdated;

        query.QueueDeviceInfos(@"invalid information which shouldn't be parsed
myandroid1 device
myandroid2 device
");
        query.UpdateConnectedDevicesList(notUsed);

        var devices = query.Devices.Values.ToArray();
        Assert.AreEqual(2, devices.Length);
        Assert.AreEqual(devices[0].Id, "myandroid1");
        Assert.AreEqual(devices[0].State, IAndroidLogcatDevice.DeviceState.Connected);
        Assert.AreEqual(devices[1].Id, "myandroid2");
        Assert.AreEqual(devices[1].State, IAndroidLogcatDevice.DeviceState.Connected);
        Assert.AreEqual(m_UpdatedCount, 1);

        query.QueueDeviceInfos("");
        query.UpdateConnectedDevicesList(notUsed);

        Assert.AreEqual(devices[0].State, IAndroidLogcatDevice.DeviceState.Disconnected);
        Assert.AreEqual(devices[1].State, IAndroidLogcatDevice.DeviceState.Disconnected);
        Assert.AreEqual(m_UpdatedCount, 2);


        query.QueueDeviceInfos(@"invalid information which shouldn't be parsed
myandroid1 device
myandroid3 offline
");
        query.UpdateConnectedDevicesList(notUsed);

        devices = query.Devices.Values.ToArray();

        Assert.AreEqual(3, devices.Length);
        Assert.AreEqual(devices[0].Id, "myandroid1");
        Assert.AreEqual(devices[0].State, IAndroidLogcatDevice.DeviceState.Connected);
        Assert.AreEqual(devices[1].Id, "myandroid2");
        Assert.AreEqual(devices[1].State, IAndroidLogcatDevice.DeviceState.Disconnected);
        Assert.AreEqual(devices[2].Id, "myandroid3");
        Assert.AreEqual(devices[2].State, IAndroidLogcatDevice.DeviceState.Disconnected);
        Assert.AreEqual(m_UpdatedCount, 3);

        // Trying to select disconected device does nothing
        query.SelectDevice(devices[1]);
        Assert.AreEqual(0, m_SelectedCount);
        Assert.AreEqual(null, query.SelectedDevice);

        query.SelectDevice(devices[0]);
        Assert.AreEqual(1, m_SelectedCount);
        Assert.AreEqual(devices[0], query.SelectedDevice);

        // No devices, selected device should deselect
        query.QueueDeviceInfos("");
        query.UpdateConnectedDevicesList(notUsed);

        Assert.AreEqual(null, query.SelectedDevice);
        Assert.AreEqual(2, m_SelectedCount);

        ShutdownRuntime();
    }

    private void Query_DevicesUpdated()
    {
        m_UpdatedCount++;
    }

    private void Query_DeviceSelected(IAndroidLogcatDevice obj)
    {
        m_SelectedCount++;
    }

    [Test]
    public void DeviceBehavesProperlyWithBaseNullDevice()
    {
        var device = new AndroidLogcatDevice(null, "test");
        var properties = typeof(AndroidLogcatDevice).GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        foreach (var p in properties)
        {
            try
            {
                var value = p.GetValue(device);
            }
            catch
            {
                Assert.Fail($"Failed to query {p.DeclaringType.Name}.{p.Name}");
            }
        }

        Assert.AreEqual("test", device.Id);
    }

    [Test]
    public void DeviceHasProperConnectionType()
    {
        var networkDevice1 = new AndroidLogcatFakeDevice90("mydevice:12345");
        var networkDevice2 = new AndroidLogcatFakeDevice90("mydevice:5555");
        var usbDevice1 = new AndroidLogcatFakeDevice90("mydevice");
        // Since the port number is over 65535, it's treated as USB device
        var usbDevice2 = new AndroidLogcatFakeDevice90("mydevice:123456");
        Assert.AreEqual(IAndroidLogcatDevice.DeviceConnectionType.Network, networkDevice1.ConnectionType);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceConnectionType.Network, networkDevice2.ConnectionType);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceConnectionType.USB, usbDevice1.ConnectionType);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceConnectionType.USB, usbDevice2.ConnectionType);
    }
}
