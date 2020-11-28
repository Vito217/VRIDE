using NUnit.Framework;
using System.Text.RegularExpressions;
using Unity.Android.Logcat;

using UnityEngine;
using UnityEditor;
using System;
#if UNITY_ANDROID
[InitializeOnLoad]
public class AndroidLogcatTestsSetup
{
    /// <summary>
    /// Set SKD/NDK for Bokken images
    /// </summary>
    static AndroidLogcatTestsSetup()
    {
#if UNITY_2019_3_OR_NEWER
        var sdkPath = Environment.GetEnvironmentVariable("ANDROID_SDK_ROOT");
        if (sdkPath != string.Empty)
        {
            UnityEditor.Android.AndroidExternalToolsSettings.sdkRootPath = sdkPath;
            Debug.Log($"SDK Path was set from ANDROID_SDK_ROOT = {sdkPath}");
        }
        else
        {
            Debug.LogWarning($"ANDROID_SDK_ROOT was not set.\nCurrently using SDK from here: {UnityEditor.Android.AndroidExternalToolsSettings.sdkRootPath}");
        }

        var ndkPath = Environment.GetEnvironmentVariable("ANDROID_NDK_ROOT");
        if (ndkPath != string.Empty)
        {
            UnityEditor.Android.AndroidExternalToolsSettings.ndkRootPath = ndkPath;
            Debug.Log($"SDK Path was set from ANDROID_NDK_ROOT = {ndkPath}");
        }
        else
        {
            Debug.LogWarning($"ANDROID_NDK_ROOT was not set.\nCurrently using NDK from here: {UnityEditor.Android.AndroidExternalToolsSettings.ndkRootPath}");
        }
#endif
    }

    public static bool AndroidSDKAndNDKAvailable()
    {
#if UNITY_2019_3_OR_NEWER
        // The only Bokken agents which have android NDK are mobile/android-execution-r19, and those are only Windows currently
        return Application.platform == RuntimePlatform.WindowsEditor;
#else
        return false;
#endif
    }
}
#endif
