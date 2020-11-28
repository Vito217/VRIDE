#if PLATFORM_ANDROID
using System;
using UnityEngine;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatStatusBar
    {
        public string Message { set; get; }

        public bool Connected { set; get; }

        public AndroidLogcatStatusBar()
        {
            Message = String.Empty;
        }

        public void DoGUI()
        {
            var rc = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.statusLabel, GUILayout.ExpandWidth(true));
            if (Event.current.type == EventType.Repaint)
            {
                AndroidLogcatStyles.statusBarBackground.Draw(rc, false, true, false, false);
            }
            rc.x += 10.0f;
            var msg = Connected ? "<color=#00FF00FF><b>Connected</b></color>" : "<color=#FF0000FF><b>Disconnected</b></color>";

            if (!string.IsNullOrEmpty(Message))
            {
                msg += " : ";
                msg += Message;
            }

            GUI.Label(rc, msg, AndroidLogcatStyles.statusLabel);
        }
    }
}
#endif
