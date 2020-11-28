#if PLATFORM_ANDROID
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Unity.Android.Logcat
{
    internal class AndroidLogcatRegexList : AndroidLogcatReordableList
    {
        private AndroidLogcatRuntimeBase m_Runtime;

        public AndroidLogcatRegexList(List<ReordableListItem> dataSource, AndroidLogcatRuntimeBase runtime) :
            base(dataSource)
        {
            ShowResetGUI = true;
            m_Runtime = runtime;
        }

        protected override void OnResetButtonClicked()
        {
            m_Runtime.Settings.ResetStacktraceResolveRegex();
        }

        protected override string ValidateItem(string item)
        {
            if (string.IsNullOrEmpty(item))
                return string.Empty;

            try
            {
                Regex.Match("", item);
            }
            catch (ArgumentException ex)
            {
                return ex.Message;
            }

            return string.Empty;
        }
    }
}
#endif
