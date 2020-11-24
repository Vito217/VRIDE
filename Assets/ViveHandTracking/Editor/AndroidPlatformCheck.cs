using UnityEditor;
using UnityEngine;
using System;
using System.IO;
using System.Linq;

namespace ViveHandTracking {

[InitializeOnLoad]
class AndroidPlatformCheck {
  private const string GoogleVRDefine = "VIVEHANDTRACKING_WITH_GOOGLEVR";
  private const string WaveVRDefine = "VIVEHANDTRACKING_WITH_WAVEVR";
  private const string WaveVR3Define = "VIVEHANDTRACKING_WITH_WAVEVR3";
  private const string WaveVRHandDefine = "VIVEHANDTRACKING_WAVEVR_HAND";
  private static string IgnoreFilePath;

  static AndroidPlatformCheck() {
    EditorApplication.update += Check;
  }

  static void Check() {
    IgnoreFilePath = Application.dataPath + "/../ViveHandTrackingSkipPlatformCheck.txt";
    EditorApplication.update -= Check;
    if (File.Exists(IgnoreFilePath))
      return;

    // check if GoogleVR and WaveVR plugin exist
    var assemblies = AppDomain.CurrentDomain.GetAssemblies();
    var types = assemblies.SelectMany(a => a.GetTypes()).ToList();
    bool hasGooglevrPlugin = types.Any(t => t.FullName == "GvrSettings");
    bool hasWavevrPlugin = types.Any(t => t.FullName == "WaveVR_Render");
    bool isWaveVR3OrNewer = hasWavevrPlugin && types.Any(t => t.FullName == "WaveVR_ButtonList");
    bool hasWavevrHand = hasWavevrPlugin && types.Any(t => t.FullName == "wvr.WVR_HandSkeletonData_t");
    if (hasGooglevrPlugin && hasWavevrPlugin) {
      bool showDialog = EditorPrefs.GetBool("ViveHandTracking.AndroidPlatformCheck.ShowDialog", true);
      if (showDialog) {
        bool result = EditorUtility.DisplayDialog(
                        "Your Project continas both GoogleVR and WaveVR plugin",
                        "Both plugins cannot work together and Vive Hand Tracking plugin cannot determine which API to use," +
                        "Please add " + GoogleVRDefine + " or " + WaveVRDefine + " to android scripting define symbols manually.",
                        "Got it", "Skip Checks"
                      );
        if (!result)
          File.WriteAllText(IgnoreFilePath, "");
      } else
        Debug.LogWarningFormat("Vive Hand Tracking detected both GoogleVR and WaveVR plugin, please add {0} or {1} to android scripting define symbols manually.",
                               GoogleVRDefine, WaveVRDefine);
      return;
    }

    // update symbols
    string symbols = PlayerSettings.GetScriptingDefineSymbolsForGroup(BuildTargetGroup.Android);
    string newSymbols = "";
    foreach (var define in symbols.Split(';')) {
      if (define == GoogleVRDefine) {
        if (!hasGooglevrPlugin)
          continue;
        hasGooglevrPlugin = false;
      } else if (define == WaveVRDefine) {
        if (!hasWavevrPlugin)
          continue;
        hasWavevrPlugin = false;
      } else if (define == WaveVR3Define) {
        if (!isWaveVR3OrNewer)
          continue;
        isWaveVR3OrNewer = false;
      } else if (define == WaveVRHandDefine) {
        if (!hasWavevrHand)
          continue;
        hasWavevrHand = false;
      }
      AppendDefine(ref newSymbols, define);
    }
    if (hasGooglevrPlugin) {
      AppendDefine(ref newSymbols, GoogleVRDefine);
      Debug.LogFormat("Add scripting define symbol {0} for Android platform", GoogleVRDefine);
    }
    if (hasWavevrPlugin) {
      AppendDefine(ref newSymbols, WaveVRDefine);
      Debug.LogFormat("Add scripting define symbol {0} for Android platform", WaveVRDefine);
    }
    if (isWaveVR3OrNewer) {
      AppendDefine(ref newSymbols, WaveVR3Define);
      Debug.LogFormat("Add scripting define symbol {0} for Android platform", WaveVR3Define);
    }
    if (hasWavevrHand) {
      AppendDefine(ref newSymbols, WaveVRHandDefine);
      Debug.LogFormat("Add scripting define symbol {0} for Android platform", WaveVRHandDefine);
    }
    PlayerSettings.SetScriptingDefineSymbolsForGroup(BuildTargetGroup.Android, newSymbols);
  }

  static void AppendDefine(ref string defines, string element) {
    if (defines != "")
      defines += ";";
    defines += element;
  }

  internal static bool BuildWithWaveVR {
    get {
#if VIVEHANDTRACKING_WAVEXR_HAND
      return true;
#else
      return PlayerSettings.GetScriptingDefineSymbolsForGroup(BuildTargetGroup.Android)
             .Split(';').Any(d => d == WaveVRDefine);
#endif
    }
  }
}

}
