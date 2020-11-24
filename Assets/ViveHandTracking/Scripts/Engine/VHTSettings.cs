using System.Collections.Generic;
using System.IO;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

namespace ViveHandTracking {

public class VHTSettings : ScriptableObject {
  public const string SettingsPath = "Assets/ViveHandTracking/Resources/VHTSettings.asset";

  public List<HandTrackingEngine> Engines = null;

  public static VHTSettings GetSettings(bool create) {
#if UNITY_EDITOR
    var settings = AssetDatabase.LoadAssetAtPath<VHTSettings>(SettingsPath);
    if (settings == null && create)
      settings = CreateDefault();
#else
    var settings = Resources.Load<VHTSettings>("VHTSettings");
    if (settings == null)
      settings = CreateDefault();
#endif
    return settings;
  }

  static VHTSettings CreateDefault() {
    var settings = ScriptableObject.CreateInstance<VHTSettings>();
    settings.Engines = new List<HandTrackingEngine>();
    settings.Engines.Add(ScriptableObject.CreateInstance<ViveHandTrackingEngine>());
    settings.Engines.Add(ScriptableObject.CreateInstance<WaveVRHandEngine>());
    settings.Engines.Add(ScriptableObject.CreateInstance<EditorEngine>());
#if UNITY_EDITOR
    if (!AssetDatabase.IsValidFolder("Assets/ViveHandTracking"))
      AssetDatabase.CreateFolder("Assets", "ViveHandTracking");
    if (!AssetDatabase.IsValidFolder("Assets/ViveHandTracking/Resources"))
      AssetDatabase.CreateFolder("Assets/ViveHandTracking", "Resources");
    AssetDatabase.CreateAsset(settings, SettingsPath);
    foreach (var engine in settings.Engines) {
      engine.name = engine.GetType().Name;
      AssetDatabase.AddObjectToAsset(engine, settings);
    }
    AssetDatabase.SaveAssets();
#endif
    return settings;
  }

#if UNITY_EDITOR
  public void Save() {
    EditorUtility.SetDirty(this);
    foreach (var engine in Engines) {
      if (!AssetDatabase.Contains(engine))
        AssetDatabase.AddObjectToAsset(engine, this);
    }
    AssetDatabase.SaveAssets();
  }
#endif
}

}
