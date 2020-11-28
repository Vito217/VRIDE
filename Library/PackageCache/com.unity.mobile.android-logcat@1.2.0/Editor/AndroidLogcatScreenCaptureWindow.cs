using System;
using System.IO;
using UnityEngine;
using UnityEditor;
using System.Linq;
#if PLATFORM_ANDROID
using UnityEditor.Android;
#endif

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatScreenCaptureWindow : EditorWindow
    {
        [SerializeField] private string m_ImagePath;
#if PLATFORM_ANDROID
        private AndroidLogcatRuntimeBase m_Runtime;
        private GUIContent[] m_Devices;
        private int m_SelectedDevice;
        private Texture2D m_ImageTexture = null;
        private int m_CaptureCount;
        private const int kButtonAreaHeight = 30;
        private const int kBottomAreaHeight = 8;
        private string m_Error;

        internal class AndroidLogcatCaptureScreenCaptureInput : IAndroidLogcatTaskInput
        {
            internal ADB adb;
            internal string deviceId;
        }

        internal class AndroidLogcatCaptureScreenCaptureResult : IAndroidLogcatTaskResult
        {
            internal string imagePath;
            internal string error;
        }

        public static void ShowWindow()
        {
            AndroidLogcatScreenCaptureWindow win = EditorWindow.GetWindow<AndroidLogcatScreenCaptureWindow>("Device Screen Capture");
            win.QueueScreenCapture();
        }

        private void OnEnable()
        {
            m_Runtime = AndroidLogcatManager.instance.Runtime;
            m_Runtime.DeviceQuery.DevicesUpdated += DeviceQuery_DevicesUpdated;

            DeviceQuery_DevicesUpdated();

            if (m_Runtime.DeviceQuery.SelectedDevice != null)
            {
                var id = m_Runtime.DeviceQuery.SelectedDevice.Id;
                for (int i = 0; i < m_Devices.Length; i++)
                {
                    if (id == m_Devices[i].text)
                    {
                        m_SelectedDevice = i;
                        break;
                    }
                }
            }
        }

        private void OnDisable()
        {
            m_Runtime.DeviceQuery.DevicesUpdated -= DeviceQuery_DevicesUpdated;
            m_SelectedDevice = 0;
        }

        private void DeviceQuery_DevicesUpdated()
        {
            m_Devices = m_Runtime.DeviceQuery.Devices.Where(m => m.Value.State == IAndroidLogcatDevice.DeviceState.Connected)
                .Select(m => new GUIContent(m.Value.Id)).ToArray();
        }

        private string GetDeviceId()
        {
            if (m_SelectedDevice < 0 || m_SelectedDevice > m_Devices.Length - 1)
                return string.Empty;
            return m_Devices[m_SelectedDevice].text;
        }

        private void QueueScreenCapture()
        {
            var id = GetDeviceId();
            if (string.IsNullOrEmpty(id))
                return;

            m_Runtime.Dispatcher.Schedule(
                new AndroidLogcatCaptureScreenCaptureInput() { adb = m_Runtime.Tools.ADB, deviceId = id},
                ExecuteScreenCapture,
                IntegrateCaptureScreenShot,
                false);
            m_CaptureCount++;
        }

        private static IAndroidLogcatTaskResult ExecuteScreenCapture(IAndroidLogcatTaskInput input)
        {
            var i = (AndroidLogcatCaptureScreenCaptureInput)input;
            string error;
            var path = AndroidLogcatUtilities.CaptureScreen(i.adb, i.deviceId, out error);

            return new AndroidLogcatCaptureScreenCaptureResult()
            {
                imagePath = path,
                error = error
            };
        }

        private void IntegrateCaptureScreenShot(IAndroidLogcatTaskResult result)
        {
            if (m_CaptureCount > 0)
                m_CaptureCount--;
            var captureResult = (AndroidLogcatCaptureScreenCaptureResult)result;
            m_ImagePath = captureResult.imagePath;
            m_Error = captureResult.error;
            if (!string.IsNullOrEmpty(m_ImagePath))
                LoadImage();
            Repaint();
        }

        void OnGUI()
        {
            EditorGUILayout.BeginVertical();
            GUILayout.Space(5);

            EditorGUILayout.BeginHorizontal(AndroidLogcatStyles.toolbar);

            GUIContent statusIcon = GUIContent.none;
            if (m_CaptureCount > 0)
            {
                int frame = (int)Mathf.Repeat(Time.realtimeSinceStartup * 10, 11.99f);
                statusIcon = AndroidLogcatStyles.Status.GetContent(frame);
                Repaint();
            }
            GUILayout.Label(statusIcon, AndroidLogcatStyles.StatusIcon, GUILayout.Width(30));

            EditorGUI.BeginChangeCheck();
            m_SelectedDevice = EditorGUILayout.Popup(m_SelectedDevice, m_Devices, AndroidLogcatStyles.toolbarPopup);
            if (EditorGUI.EndChangeCheck())
                QueueScreenCapture();

            EditorGUI.BeginDisabledGroup(m_CaptureCount > 0);
            if (GUILayout.Button("Capture", AndroidLogcatStyles.toolbarButton))
                QueueScreenCapture();
            EditorGUI.EndDisabledGroup();

            if (GUILayout.Button("Save...", AndroidLogcatStyles.toolbarButton))
            {
                var path = EditorUtility.SaveFilePanel("Save Screen Capture", "", Path.GetFileName(m_ImagePath), "png");
                if (!string.IsNullOrEmpty(path))
                {
                    try
                    {
                        File.Copy(m_ImagePath, path, true);
                    }
                    catch (Exception ex)
                    {
                        Debug.LogErrorFormat("Failed to save to '{0}' as '{1}'.", path, ex.Message);
                    }
                }
            }
            EditorGUILayout.EndHorizontal();

            GUILayout.Space(10);
            var id = GetDeviceId();
            if (string.IsNullOrEmpty(id))
                EditorGUILayout.HelpBox("No valid device detected, please reopen this window after selecting proper device.", MessageType.Info);
            else
            {
                if (!string.IsNullOrEmpty(m_Error))
                {
                    EditorGUILayout.HelpBox(m_Error, MessageType.Error);
                }
                else
                {
                    if (m_ImageTexture != null)
                    {
                        Rect rect = new Rect(0, kButtonAreaHeight, position.width, position.height - kButtonAreaHeight - kBottomAreaHeight);
                        GUI.DrawTexture(rect, m_ImageTexture, ScaleMode.ScaleToFit);
                    }
                }
            }
            EditorGUILayout.EndVertical();
        }

        void LoadImage()
        {
            if (!File.Exists(m_ImagePath))
                return;

            byte[] imageData;
            imageData = File.ReadAllBytes(m_ImagePath);

            m_ImageTexture = new Texture2D(2, 2); // The size will be replaced by LoadImage().
            if (!m_ImageTexture.LoadImage(imageData))
                return;

            maxSize = new Vector2(Math.Max(m_ImageTexture.width, position.width), m_ImageTexture.height + kButtonAreaHeight);
        }

#else
        internal void OnGUI()
        {
            AndroidLogcatUtilities.ShowActivePlatformNotAndroidMessage();
        }

#endif
    }
}
