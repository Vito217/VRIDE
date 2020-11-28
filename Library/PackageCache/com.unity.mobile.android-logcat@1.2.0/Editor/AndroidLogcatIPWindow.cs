#if PLATFORM_ANDROID
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using UnityEditor.Android;
using System.Text.RegularExpressions;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatIPWindow : EditorWindow
    {
        internal static Regex kIPRegex = new Regex(@"src\s+(?<ip>\d+\.\d+\.\d+\.\d+)");
        private AndroidLogcatRuntimeBase m_Runtime;
        internal string m_IpString;
        internal string m_PortString;
        private Vector2 m_DevicesScrollPosition = Vector2.zero;
        private Rect m_DeviceScrollRect = new Rect();

        private const string kAndroidLogcatLastIp = "AndroidLogcatLastIp";
        private const string kAndroidLogcatLastPort = "AndroidLogcatLastPort";

        private GUIContent kConnect = new GUIContent(L10n.Tr("Connect"), L10n.Tr("Sets the target device to listen for a TCP/IP connection on port 5555 and connects to it via IP address."));
        private GUIContent kDisconnect = new GUIContent(L10n.Tr("Disconnect"));

        public static void Show(AndroidLogcatRuntimeBase runtime, Rect screenRect)
        {
            AndroidLogcatIPWindow win = EditorWindow.GetWindow<AndroidLogcatIPWindow>(true, "Other connection options");
            win.position = new Rect(screenRect.x, screenRect.y, 700, 200);
        }

        void OnEnable()
        {
            if (m_Runtime == null)
                m_Runtime = AndroidLogcatManager.instance.Runtime;
            m_IpString = EditorPrefs.GetString(kAndroidLogcatLastIp, "");
            m_PortString = EditorPrefs.GetString(kAndroidLogcatLastPort, "5555");

            m_Runtime.DeviceQuery.DevicesUpdated += DevicesUpdated;

            // Disable progress bar just in case, if we have a stale process hanging where we peform adb connect
            EditorUtility.ClearProgressBar();
        }

        private void OnDisable()
        {
            if (m_Runtime == null)
                return;
            m_Runtime.DeviceQuery.DevicesUpdated -= DevicesUpdated;
        }

        private void DevicesUpdated()
        {
            Repaint();
        }

        /// <summary>
        /// Connect to the device by ip address.
        /// Please refer to https://developer.android.com/studio/command-line/adb#wireless for details.
        /// </summary>
        /// <param name="ip"> The ip address of the device that needs to be connected. Port can be included like 'device_ip_address:port'. Both IPV4 and IPV6 are supported. </param>
        public  void ConnectDevice(string ip, string port)
        {
            EditorUtility.DisplayProgressBar("Connecting", "Connecting to " + ip + ":" + port, 0.0f);
            m_Runtime.Dispatcher.Schedule(new AndroidLogcatConnectToDeviceInput() { adb = m_Runtime.Tools.ADB, ip = ip, port = port}, AndroidLogcatConnectToDeviceTask.Execute, IntegrateConnectToDevice, false);
        }

        public void SetTCPIPAndConnectDevice(string deviceId, string ip, string port)
        {
            EditorUtility.DisplayProgressBar("Connecting",
                string.Join("\n", new string[]
                {
                    "Set listening port to " + port + ". Connecting to " + ip + ":" + port,
                }), 0.0f);
            m_Runtime.Dispatcher.Schedule(new AndroidLogcatConnectToDeviceInput() { adb = m_Runtime.Tools.ADB, ip = ip, port = port, deviceId = deviceId, setListeningPort = true }, AndroidLogcatConnectToDeviceTask.Execute, IntegrateConnectToDevice, false);
        }

        private void DisconnectDevice(IAndroidLogcatDevice device)
        {
            var command = "disconnect " + device.Id;
            AndroidLogcatInternalLog.Log("adb " + command);
            var result = m_Runtime.Tools.ADB.Run(new[] { command }, "Failed to disconnect " + device.Id);
            AndroidLogcatInternalLog.Log(result);
        }

        private static void IntegrateConnectToDevice(IAndroidLogcatTaskResult result)
        {
            var r = (AndroidLogcatConnectToDeviceResult)result;
            AndroidLogcatInternalLog.Log(r.message);
            EditorUtility.ClearProgressBar();
            EditorUtility.DisplayDialog(r.success ? "Success" : "Failure", r.message, "Ok");
        }

        internal static string ParseIPAddress(string input)
        {
            var result = kIPRegex.Match(input);
            if (result.Success)
                return result.Groups["ip"].Value;
            return null;
        }

        string CopyIP(string deviceId)
        {
            var command = "-s " + deviceId + " shell ip route";
            AndroidLogcatInternalLog.Log("adb " + command);
            var result = m_Runtime.Tools.ADB.Run(new[] { command }, "Failed to query ip");
            AndroidLogcatInternalLog.Log(result);
            var ip = ParseIPAddress(result);
            return string.IsNullOrEmpty(ip) ? "Failed to get IP address" : ip;
        }

        void OnGUI()
        {
            EditorGUILayout.BeginVertical();
            {
                EditorGUILayout.LabelField("Available devices:", EditorStyles.boldLabel);
                GUI.Box(m_DeviceScrollRect, GUIContent.none, EditorStyles.helpBox);
                m_DevicesScrollPosition = EditorGUILayout.BeginScrollView(m_DevicesScrollPosition);
                bool refreshDevices = false;
                foreach (var deviceValue in m_Runtime.DeviceQuery.Devices)
                {
                    var device = deviceValue.Value;
                    EditorGUILayout.BeginHorizontal();
                    EditorGUILayout.LabelField(device.DisplayName, EditorStyles.label);

                    EditorGUI.BeginDisabledGroup(device.State != IAndroidLogcatDevice.DeviceState.Connected);
                    if (GUILayout.Button(" Copy IP ", GUILayout.ExpandWidth(false)))
                    {
                        m_IpString = CopyIP(device.Id);
                        EditorGUIUtility.systemCopyBuffer = m_IpString;
                        GUIUtility.keyboardControl = 0;
                        GUIUtility.hotControl = 0;
                        Repaint();
                    }

                    float connectButtoSize = 100.0f;
                    if (device.ConnectionType == IAndroidLogcatDevice.DeviceConnectionType.Network)
                    {
                        if (GUILayout.Button(kDisconnect, GUILayout.Width(connectButtoSize)))
                        {
                            DisconnectDevice(device);
                            refreshDevices = true;
                        }
                    }
                    else
                    {
                        if (GUILayout.Button(kConnect, GUILayout.Width(connectButtoSize)))
                        {
                            SetTCPIPAndConnectDevice(device.Id, CopyIP(device.Id), "5555");
                            refreshDevices = true;
                        }
                    }
                    EditorGUI.EndDisabledGroup();

                    var rc = GUILayoutUtility.GetLastRect();
                    var orgColor = GUI.color;
                    GUI.color = Color.black;
                    if (Event.current.type == EventType.Repaint)
                        GUI.DrawTexture(new Rect(0, rc.y + rc.height, m_DeviceScrollRect.width, 1), EditorGUIUtility.whiteTexture);
                    GUI.color = orgColor;
                    EditorGUILayout.EndHorizontal();

                    if (refreshDevices)
                    {
                        m_Runtime.DeviceQuery.UpdateConnectedDevicesList(true);
                        GUIUtility.keyboardControl = 0;
                        GUIUtility.hotControl = 0;
                        break;
                    }
                }
                EditorGUILayout.EndScrollView();
                if (Event.current.type == EventType.Repaint)
                    m_DeviceScrollRect = GUILayoutUtility.GetLastRect();
                GUILayout.Space(5);
                EditorGUILayout.BeginHorizontal();
                EditorGUILayout.LabelField("IP", EditorStyles.boldLabel);
                EditorGUILayout.LabelField("Port", EditorStyles.boldLabel, GUILayout.Width(100));
                EditorGUILayout.EndHorizontal();
                EditorGUILayout.BeginHorizontal();
                m_IpString = EditorGUILayout.TextField(m_IpString);
                m_PortString = EditorGUILayout.TextField(m_PortString, GUILayout.Width(100));
                EditorGUILayout.EndHorizontal();

                EditorGUILayout.BeginHorizontal();

                EditorGUI.BeginDisabledGroup(string.IsNullOrEmpty(m_IpString));
                if (GUILayout.Button("Connect"))
                {
                    Close();
                    EditorPrefs.SetString(kAndroidLogcatLastIp, m_IpString);
                    EditorPrefs.SetString(kAndroidLogcatLastPort, m_PortString);
                    ConnectDevice(m_IpString, m_PortString);
                    GUIUtility.ExitGUI();
                }
                EditorGUI.EndDisabledGroup();
                if (GUILayout.Button("Refresh Devices"))
                {
                    m_Runtime.DeviceQuery.UpdateConnectedDevicesList(false);
                }

                EditorGUILayout.EndHorizontal();
            }
            EditorGUILayout.EndVertical();
        }
    }
}
#endif
