#if PLATFORM_ANDROID
using UnityEditor;
using UnityEditor.Build;
using UnityEditor.Build.Reporting;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatCallbacks : IPostprocessBuildWithReport
    {
        public int callbackOrder { get { return 0; } }

        public void OnPostprocessBuild(BuildReport report)
        {
            if ((report.summary.options & BuildOptions.AutoRunPlayer) != 0 && AndroidLogcatConsoleWindow.ShowDuringBuildRun)
                AndroidLogcatConsoleWindow.ShowNewOrExisting(true);
        }
    }
}
#endif
