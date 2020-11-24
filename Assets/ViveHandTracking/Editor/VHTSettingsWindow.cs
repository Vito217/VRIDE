using System;
using System.Linq;
using UnityEngine;
using UnityEditor;
using UnityEditorInternal;

namespace ViveHandTracking {

[InitializeOnLoad]
class VHTSettingsWindow : EditorWindow {
  static VHTSettingsWindow() {
    EditorApplication.update += Update;
  }

  static void Update() {
    var settings = VHTSettings.GetSettings(false);
    if (settings == null)
      ShowWindow();
    EditorApplication.update -= Update;
  }

  [UnityEditor.MenuItem("Vive Hand Tracking/Settings")]
  static void ShowWindow() {
    var window = GetWindow<VHTSettingsWindow>(true);
    window.titleContent = new GUIContent("Vive Hand Tracking Settings");
    window.minSize = new Vector2(500, 250);
  }

  VHTSettingsDrawer drawer = null;
  private VHTSettings settings = null;
  private Vector2 scrollPosition;

  void OnGUI() {
    if (settings == null)
      settings = VHTSettings.GetSettings(true);
    if (drawer == null)
      drawer = new VHTSettingsDrawer(new SerializedObject(settings));
    GUILayout.BeginScrollView(scrollPosition);
    drawer.Draw();
    GUILayout.FlexibleSpace();
    if (GUILayout.Button("Save"))
      settings.Save();
    GUILayout.EndScrollView();
  }
}
}
