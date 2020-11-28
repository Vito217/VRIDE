#if PLATFORM_ANDROID
namespace Unity.Android.Logcat
{
    internal struct BuildInfo
    {
        public string buildType;
        public string scriptingImplementation;
        public string cpu;
    }
}
#endif
