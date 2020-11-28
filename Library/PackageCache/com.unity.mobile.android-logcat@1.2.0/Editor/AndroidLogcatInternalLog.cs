using System;
using System.Linq;
using UnityEngine;
using System.Text;
using UnityEditor;

namespace Unity.Android.Logcat
{
#if PLATFORM_ANDROID
    internal class AndroidLogcatInternalLog : EditorWindow
    {
        static AndroidLogcatInternalLog ms_Instance = null;
        static StringBuilder ms_LogEntries = new StringBuilder();

        Vector2 m_ScrollPosition = Vector2.zero;
        public static void ShowLog(bool immediate)
        {
            if (ms_Instance == null)
                ms_Instance = ScriptableObject.CreateInstance<AndroidLogcatInternalLog>();

            ms_Instance.titleContent = new GUIContent("Internal Log");
            ms_Instance.Show(immediate);
            ms_Instance.Focus();
        }

        /// <summary>
        /// This function should be thread safe.
        /// </summary>
        /// <param name="message"></param>
        /// <param name="args"></param>
        public static void Log(string message, params object[] args)
        {
            lock (ms_LogEntries)
            {
                var timedMessage = AndroidLogcatDispatcher.isMainThread ? "[MainThread]" : "[WorkThread] ";
                timedMessage += DateTime.Now.ToString("HH:mm:ss.ffff") + " " + string.Format(message, args);
                ms_LogEntries.AppendLine(timedMessage);

                Console.WriteLine("[Logcat] " + timedMessage);
            }

            if (AndroidLogcatDispatcher.isMainThread && ms_Instance != null)
            {
                ms_Instance.m_ScrollPosition = new Vector2(ms_Instance.m_ScrollPosition.x, float.MaxValue);
                ms_Instance.Repaint();
            }
        }

        public void OnEnable()
        {
            ms_Instance = this;
        }

        public void OnGUI()
        {
            GUILayout.BeginHorizontal();
            int count;
            lock (ms_LogEntries)
            {
                count = ms_LogEntries.Length;
            }
            GUILayout.Label("Entries: " + count);
            if (GUILayout.Button("Clear"))
            {
                lock (ms_LogEntries)
                {
                    ms_LogEntries = new StringBuilder();
                }
            }
            GUILayout.EndHorizontal();
            var e = Event.current;
            if (e.type == EventType.MouseDown && e.button == 1)
            {
                var menuItems = new[] { new GUIContent("Copy All") };
                EditorUtility.DisplayCustomMenu(new Rect(e.mousePosition.x, e.mousePosition.y, 0, 0), menuItems.ToArray(), -1, MenuSelection, null);
            }

            m_ScrollPosition = GUILayout.BeginScrollView(m_ScrollPosition, true, true);
            GUILayout.TextArea(ms_LogEntries.ToString(), AndroidLogcatStyles.internalLogStyle, GUILayout.ExpandHeight(true));
            GUILayout.EndScrollView();
        }

        private void MenuSelection(object userData, string[] options, int selected)
        {
            switch (selected)
            {
                // Copy All
                case 0:
                    EditorGUIUtility.systemCopyBuffer = ms_LogEntries.ToString();
                    break;
            }
        }
    }
#else
    internal class AndroidLogcatInternalLog : EditorWindow
    {
        internal void OnGUI()
        {
            AndroidLogcatUtilities.ShowActivePlatformNotAndroidMessage();
        }
    }
#endif
}
