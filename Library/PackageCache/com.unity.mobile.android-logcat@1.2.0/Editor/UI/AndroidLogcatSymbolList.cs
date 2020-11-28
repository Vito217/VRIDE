#if PLATFORM_ANDROID
using System.Collections.Generic;
using System.IO;
using UnityEditor;
using UnityEngine;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatSymbolList : AndroidLogcatReordableList
    {
        public AndroidLogcatSymbolList(List<ReordableListItem> dataSource) : base(dataSource)
        {
            ShowEntryGUI = false;
        }

        protected override void OnPlusButtonClicked()
        {
            var backends = new[] {"mono", "il2cpp"};
            var buildTypes = new[] {"Debug", "Development", "Release"};
            var cpu = new[] {"armeabi-v7a", "arm64-v8a"};
            var names = new List<GUIContent>();
            var paths = new List<string>();

            var engineDirectory = BuildPipeline.GetPlaybackEngineDirectory(BuildTarget.Android, BuildOptions.None);

            foreach (var b in backends)
            {
                foreach (var t in buildTypes)
                {
                    foreach (var c in cpu)
                    {
                        var path = Path.GetFullPath(Paths.Combine(engineDirectory, "Variations", b, t, "Symbols", c));
                        if (!Directory.Exists(path))
                            continue;

                        names.Add(new GUIContent($"{Application.unityVersion}/{b}/{t}/{c}"));
                        paths.Add(path);
                    }
                }
            }

            names.Add(GUIContent.none);
            paths.Add(string.Empty);

            names.Add(new GUIContent("Pick Custom Location"));
            paths.Add(string.Empty);

            int selected = -1;
            var m = Event.current.mousePosition;
            EditorUtility.DisplayCustomMenu(new Rect(m, Vector2.zero), names.ToArray(), selected, DoSymbolPickSelection,
                paths);
        }

        protected override void DoListGUIWhenEmpty()
        {
            EditorGUILayout.HelpBox("Please add directories containing symbols for your native libraries.",
                MessageType.Info, true);
        }

        private void DoSymbolPickSelection(object userData, string[] options, int selected)
        {
            var paths = (List<string>)userData;
            var selectedPath = paths[selected];
            if (!string.IsNullOrEmpty(selectedPath))
            {
                GUIUtility.keyboardControl = 0;
                AddItem(selectedPath);
                return;
            }

            var item = EditorUtility.OpenFolderPanel("Locate symbol path", CurrentItemName, "");
            if (string.IsNullOrEmpty(item))
                return;
            GUIUtility.keyboardControl = 0;
            AddItem(item);
        }
    }
}
#endif
