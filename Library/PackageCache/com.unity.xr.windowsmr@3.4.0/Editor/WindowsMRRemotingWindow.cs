using UnityEditor;
using UnityEngine;
using UnityEngine.XR.WindowsMR;

namespace UnityEditor.XR.WindowsMR
{
    public class WindowsMRRemotingWindow : EditorWindow
    {
        [MenuItem("Window/XR/Windows XR Plugin Remoting")]
        public static void Init()
        {
            GetWindow<WindowsMRRemotingWindow>(false);
        }

        static GUIContent s_ConnectionStatusText = EditorGUIUtility.TrTextContent("Connection Status");
        static GUIContent s_EmulationModeText = EditorGUIUtility.TrTextContent("Emulation Mode");
        static GUIContent s_RemoteMachineText = EditorGUIUtility.TrTextContent("Remote Machine");
        static GUIContent s_EnableVideoText = EditorGUIUtility.TrTextContent("Enable Video");
        static GUIContent s_EnableAudioText = EditorGUIUtility.TrTextContent("Enable Audio");
        static GUIContent s_MaxBitrateText = EditorGUIUtility.TrTextContent("Max Bitrate (kbps)");

        static GUIContent s_ConnectionButtonConnectText = EditorGUIUtility.TrTextContent("Connect");
        static GUIContent s_ConnectionButtonDisconnectText = EditorGUIUtility.TrTextContent("Disconnect");

        static GUIContent s_ConnectionStateDisconnectedText = EditorGUIUtility.TrTextContent("Disconnected");
        static GUIContent s_ConnectionStateConnectingText = EditorGUIUtility.TrTextContent("Connecting");
        static GUIContent s_ConnectionStateConnectedText = EditorGUIUtility.TrTextContent("Connected");

        static GUIContent s_RemotingSettingsReminder = EditorGUIUtility.TrTextContent("The Editor uses player settings from the 'Standalone' platform for play mode and a remoting connection can be established without 'Windows Mixed Reality' enabled.");

        ConnectionState previousConnectionState = ConnectionState.Disconnected;

        static GUIContent[] s_ModeStrings = new GUIContent[]
        {
            EditorGUIUtility.TrTextContent("None"),
            EditorGUIUtility.TrTextContent("Remote to Device")
        };

        void OnEnable()
        {
            titleContent = EditorGUIUtility.TrTextContent("Windows Mixed Reality");
        }

        void DrawEmulationModeOnGUI()
        {
            EditorGUI.BeginDisabledGroup(EditorApplication.isPlayingOrWillChangePlaymode);
            EditorGUI.BeginChangeCheck();
            WindowsMREmulationMode previousMode = WindowsMREmulation.mode;
            WindowsMREmulationMode currentMode = (WindowsMREmulationMode)EditorGUILayout.Popup(s_EmulationModeText, (int)previousMode, s_ModeStrings);
            if (EditorGUI.EndChangeCheck())
            {
                if (previousMode == WindowsMREmulationMode.Remoting)
                    WindowsMRRemoting.Disconnect();
                WindowsMREmulation.mode = currentMode;
            }
            EditorGUI.EndDisabledGroup();
        }

        string m_RemoteMachineName = "";
        void DrawRemotingOnGUI()
        {
            EditorGUILayout.HelpBox(s_RemotingSettingsReminder);
            EditorGUI.BeginDisabledGroup(WindowsMRRemoting.isConnected);
            m_RemoteMachineName = EditorGUILayout.TextField(s_RemoteMachineText, m_RemoteMachineName);
            WindowsMRRemoting.remoteMachineName = m_RemoteMachineName;
            WindowsMRRemoting.isVideoEnabled = EditorGUILayout.Toggle(s_EnableVideoText, WindowsMRRemoting.isVideoEnabled);
            WindowsMRRemoting.isAudioEnabled = EditorGUILayout.Toggle(s_EnableAudioText, WindowsMRRemoting.isAudioEnabled);
            WindowsMRRemoting.maxBitRateKbps = EditorGUILayout.IntSlider(s_MaxBitrateText, WindowsMRRemoting.maxBitRateKbps, 1024, 99999);
            EditorGUI.EndDisabledGroup();

            GUIContent labelContent;
            GUIContent buttonContent;
            ConnectionState connectionState;
            if (!WindowsMRRemoting.TryGetConnectionState(out connectionState))
            {
                Debug.Log("Failed to get connection state! Exiting remoting-window drawing.");
                return;
            }

            if (previousConnectionState == ConnectionState.Connecting && connectionState == ConnectionState.Disconnected)
            {
                ConnectionFailureReason failureReason;
                WindowsMRRemoting.TryGetConnectionFailureReason(out failureReason);

                Debug.Log("Connection Failure Reason: " + failureReason);
            }

            previousConnectionState = connectionState;

            switch (connectionState)
            {
                case ConnectionState.Disconnected:
                default:
                    labelContent = s_ConnectionStateDisconnectedText;
                    buttonContent = s_ConnectionButtonConnectText;
                    break;

                case ConnectionState.Connecting:
                    labelContent = s_ConnectionStateConnectingText;
                    buttonContent = s_ConnectionButtonDisconnectText;
                    break;

                case ConnectionState.Connected:
                    labelContent = s_ConnectionStateConnectedText;
                    buttonContent = s_ConnectionButtonDisconnectText;
                    break;
            }

            EditorGUILayout.BeginHorizontal();
            EditorGUILayout.PrefixLabel(s_ConnectionStatusText, "Button");
            float iconSize = EditorGUIUtility.singleLineHeight;
            Rect iconRect = GUILayoutUtility.GetRect(iconSize, iconSize, GUILayout.ExpandWidth(false));
            EditorGUILayout.LabelField(labelContent);
            EditorGUILayout.EndHorizontal();

            EditorGUI.BeginDisabledGroup(EditorApplication.isPlayingOrWillChangePlaymode);
            bool pressed = EditorGUILayout.DropdownButton(buttonContent, FocusType.Passive, EditorStyles.miniButton);
            EditorGUI.EndDisabledGroup();

            if (pressed)
            {
                if (EditorGUIUtility.editingTextField)
                {
                    EditorGUIUtility.editingTextField = false;
                    GUIUtility.keyboardControl = 0;
                }

                HandleButtonPress();
            }
        }

        private void HandleButtonPress()
        {
            if (EditorApplication.isPlayingOrWillChangePlaymode)
            {
                Debug.LogError("Unable to connect / disconnect remoting while playing.");
                return;
            }

            ConnectionState connectionState;
            if (!WindowsMRRemoting.TryGetConnectionState(out connectionState))
            {
                Debug.LogError("Failed to get connection state - exiting button-press response!");
                return;
            }

            if (connectionState == ConnectionState.Connecting ||
                connectionState == ConnectionState.Connected)
                WindowsMRRemoting.Disconnect();
            else if (!string.IsNullOrEmpty(WindowsMRRemoting.remoteMachineName))
                WindowsMRRemoting.Connect();
            else
                Debug.LogError("Cannot connect without a remote machine name!");
        }

        void OnGUI()
        {
            DrawEmulationModeOnGUI();
            if (WindowsMREmulation.mode == WindowsMREmulationMode.Remoting)
                DrawRemotingOnGUI();
        }
    }
}
