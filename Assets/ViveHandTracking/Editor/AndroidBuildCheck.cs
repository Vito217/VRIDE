using UnityEditor;
using UnityEditor.Build;
using UnityEngine;
using System.IO;

namespace ViveHandTracking {

static class AndroidBuildCheck {
  private const string android32Lib = "Assets/ViveHandTracking/Plugins/Android/libs/armeabi-v7a/libaristo_interface.so";
  private const string android64Lib = "Assets/ViveHandTracking/Plugins/Android/libs/arm64-v8a/libaristo_interface.so";
  private const string waveVR32Lib =
    "Assets/ViveHandTracking/Plugins/Android/libs/armeabi-v7a/libaristo_interface_wavevr.so";
  private const string waveVR64Lib =
    "Assets/ViveHandTracking/Plugins/Android/libs/arm64-v8a/libaristo_interface_wavevr.so";

  private static void EnableLibrary(string lib, bool enable) {
    var plugin = AssetImporter.GetAtPath(lib) as PluginImporter;
    if (plugin != null)
      plugin.SetCompatibleWithPlatform(BuildTarget.Android, enable);
  }

  private class CustomPreprocessor : IPreprocessBuild {
    public int callbackOrder {
      get {
        return 0;
      }
    }

    public void OnPreprocessBuild(BuildTarget target, string path) {
      if (target != BuildTarget.Android)
        return;
      if (AndroidPlatformCheck.BuildWithWaveVR) {
        Debug.Log("Build with WaveVR version of Hand Tracking SDK");
        EnableLibrary(android32Lib, false);
        EnableLibrary(android64Lib, false);
        EnableLibrary(waveVR32Lib, true);
        EnableLibrary(waveVR64Lib, true);
      } else {
        Debug.Log("Build with Android version of Hand Tracking SDK");
        EnableLibrary(android32Lib, true);
        EnableLibrary(android64Lib, true);
        EnableLibrary(waveVR32Lib, false);
        EnableLibrary(waveVR64Lib, false);
      }
    }
  }
}

}
