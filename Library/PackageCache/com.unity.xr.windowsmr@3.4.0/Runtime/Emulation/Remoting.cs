using System;
using UnityEngine;
using UnityEngine.Scripting;
using UnityEngine.XR.WindowsMRInternals;

#if UNITY_EDITOR
using UnityEditor;
#endif

#if UNITY_2019_3_OR_NEWER && !UNITY_2020_2_OR_NEWER
using UnityEngineInternal.XR.WSA;
#endif

#if UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || UNITY_WINRT
using System.Runtime.InteropServices;
#endif

namespace UnityEngine.XR.WindowsMR
{
    // Must match ConnectionFlags in native code
    public enum ConnectionFlags
    {
        None = 0,
        EnableAudio = 1 << 0,
        EnableVideo = 1 << 1,
    }

    // Do not change this enum, it matches what's inside a DLL we call into
    public enum ConnectionFailureReason
    {
        None,
        Unknown,
        Unreachable,
        HandshakeFailed,
        ProtocolVersionMismatch,
        ConnectionLost,
    }

    // Do not change this enum, it matches what's inside a DLL we call into
    public enum ConnectionState
    {
        Disconnected,
        Connecting,
        Connected,
    }

    public static class WindowsMRRemoting
    {
        static WindowsMRRemoting()
        {
            WindowsMRInternal.Init();

            #if UNITY_EDITOR
            EditorApplication.playModeStateChanged += OnPlayModeStateChanged;
            #endif
        }

        #if UNITY_EDITOR
        static void OnPlayModeStateChanged(PlayModeStateChange stateChange)
        {
            if (stateChange == PlayModeStateChange.ExitingPlayMode)
                Disconnect();
        }
        #endif

        public static bool isAudioEnabled
        {
            get
            {
                return UnityWindowsMR_Remoting_IsAudioEnabled();
            }
            set
            {
                UnityWindowsMR_Remoting_SetAudioEnabled(value);
            }
        }

        public static bool isVideoEnabled
        {
            get
            {
                return UnityWindowsMR_Remoting_IsVideoEnabled();
            }
            set
            {
                UnityWindowsMR_Remoting_SetVideoEnabled(value);
            }
        }

        public static int maxBitRateKbps
        {
            get
            {
                return UnityWindowsMR_Remoting_GetMaxBitRateKbps();
            }
            set
            {
                UnityWindowsMR_Remoting_SetMaxBitRateKbps(value);
            }
        }

        public static string remoteMachineName
        {
            get
            {
                var sb = new System.Text.StringBuilder(256);
                UnityWindowsMR_Remoting_GetRemoteMachineName(sb, sb.Capacity);
                return sb.ToString();
            }
            set
            {
                UnityWindowsMR_Remoting_SetRemoteMachineName(value);
            }
        }

        public static bool isConnected
        {
            get
            {
                return UnityWindowsMR_Remoting_IsConnected();
            }
        }

        public static void Connect()
        {
#if UNITY_2019_3_OR_NEWER && !UNITY_2020_2_OR_NEWER
#pragma warning disable 0618
            RemoteSpeechAccess.EnableRemoteSpeech(UnityEngine.XR.WSA.RemoteDeviceVersion.V2);
#pragma warning restore 0618
#endif

            // throw exception on failed connection?
            Debug.Log("Remoting connect returned: " + UnityWindowsMR_Remoting_TryConnect());
        }

        public static void Disconnect()
        {
            // throw exception on failed disconnection?
            UnityWindowsMR_Remoting_TryDisconnect();

#if UNITY_2019_3_OR_NEWER && !UNITY_2020_2_OR_NEWER
#pragma warning disable 0618
            RemoteSpeechAccess.DisableRemoteSpeech();
#pragma warning restore 0618
#endif
        }

        public static bool TryGetConnectionState(out ConnectionState connectionState)
        {
            connectionState = ConnectionState.Disconnected;
            return UnityWindowsMR_Remoting_TryGetConnectionState(ref connectionState);
        }

        public static bool TryGetConnectionFailureReason(out ConnectionFailureReason connectionFailureReason)
        {
            connectionFailureReason = ConnectionFailureReason.None;
            return UnityWindowsMR_Remoting_TryGetConnectionFailureReason(ref connectionFailureReason);
        }

#if UNITY_EDITOR_WIN || UNITY_STANDALONE_WIN || UNITY_WINRT
        [DllImport("WindowsMRXRSDK")]
        public static extern void UnityWindowsMR_Remoting_SetVideoEnabled(bool video);

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_IsVideoEnabled();

        [DllImport("WindowsMRXRSDK")]
        public static extern void UnityWindowsMR_Remoting_SetAudioEnabled(bool audio);

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_IsAudioEnabled();

        [DllImport("WindowsMRXRSDK")]
        public static extern void UnityWindowsMR_Remoting_SetMaxBitRateKbps(int kbps);

        [DllImport("WindowsMRXRSDK")]
        public static extern int UnityWindowsMR_Remoting_GetMaxBitRateKbps();

        [DllImport("WindowsMRXRSDK")]
        public static extern void UnityWindowsMR_Remoting_SetRemoteMachineName([MarshalAs(UnmanagedType.LPWStr)] string clientMachineName);

        [DllImport("WindowsMRXRSDK")]
        public static extern void UnityWindowsMR_Remoting_GetRemoteMachineName(System.Text.StringBuilder sb, int capacity);

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_IsConnected();

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_TryGetConnectionState(ref ConnectionState connectionState);

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_TryGetConnectionFailureReason(ref ConnectionFailureReason connectionFailureReason);

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_TryConnect();

        [DllImport("WindowsMRXRSDK")]
        public static extern bool UnityWindowsMR_Remoting_TryDisconnect();
#else
        static void UnityWindowsMR_Remoting_SetVideoEnabled(bool video)
        {
        }

        static bool UnityWindowsMR_Remoting_IsVideoEnabled()
        {
            return false;
        }

        static void UnityWindowsMR_Remoting_SetAudioEnabled(bool audio)
        {
        }

        static bool UnityWindowsMR_Remoting_IsAudioEnabled()
        {
            return false;
        }

        static void UnityWindowsMR_Remoting_SetMaxBitRateKbps(int kbps)
        {
        }

        static int UnityWindowsMR_Remoting_GetMaxBitRateKbps()
        {
            return -1;
        }

        static void UnityWindowsMR_Remoting_SetRemoteMachineName(string clientMachineName)
        {
        }

        static void UnityWindowsMR_Remoting_GetRemoteMachineName(System.Text.StringBuilder sb, int capacity)
        {
        }

        static bool UnityWindowsMR_Remoting_IsConnected()
        {
            return false;
        }

        static bool UnityWindowsMR_Remoting_TryGetConnectionState(ref ConnectionState connectionState)
        {
            return false;
        }

        static bool UnityWindowsMR_Remoting_TryGetConnectionFailureReason(ref ConnectionFailureReason connectionFailureReason)
        {
            return false;
        }

        static bool UnityWindowsMR_Remoting_TryConnect()
        {
            return false;
        }

        static bool UnityWindowsMR_Remoting_TryDisconnect()
        {
            return false;
        }
#endif
    }
}
