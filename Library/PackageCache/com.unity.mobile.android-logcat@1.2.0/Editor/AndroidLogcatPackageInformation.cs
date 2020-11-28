#if PLATFORM_ANDROID

using System;

namespace Unity.Android.Logcat
{
    [Serializable]
    internal class PackageInformation
    {
        public string deviceId;
        public string name;
        public int processId;
        public bool exited;

        public string DisplayName
        {
            get
            {
                var result = name + " (" + processId + ")";
                if (exited)
                    result += " [Exited]";
                return result;
            }
        }

        public PackageInformation()
        {
            Reset();
        }

        public void Reset()
        {
            deviceId = string.Empty;
            name = string.Empty;
            processId = 0;
            exited = false;
        }

        public void SetExited()
        {
            exited = true;
        }

        public void SetAlive()
        {
            exited = false;
        }

        public bool IsAlive()
        {
            return !exited && processId != 0;
        }
    }
}
#endif
