#if PLATFORM_ANDROID
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEditor;
using UnityEditor.Android;
using System.Text;
using UnityEngine;


namespace Unity.Android.Logcat
{
    /// <summary>
    /// https://developer.android.com/studio/command-line/dumpsys#meminfo
    /// </summary>
    internal enum MemoryGroup
    {
        ProportionalSetSize,
        HeapAlloc,
        HeapSize
    }

    internal enum MemoryType
    {
        Unknown = -1,
        NativeHeap = 0,
        JavaHeap = 1,
        Code = 2,
        Stack = 3,
        Graphics = 4,
        PrivateOther = 5,
        System = 6,
        Total = 7,
        TotalSwapPss = 8
    }


    internal class AndroidMemoryStatistics
    {
        const UInt64 kOneKiloByte = 1000;
        private MemoryGroup[] m_MemoryGroups = (MemoryGroup[])Enum.GetValues(typeof(MemoryGroup));
        private Dictionary<MemoryType, UInt64>[] m_Data = new Dictionary<MemoryType, UInt64>[Enum.GetValues(typeof(MemoryGroup)).Length];

        private Dictionary<MemoryType, UInt64> GetPSSMemoryGroup()
        {
            return m_Data[(int)MemoryGroup.ProportionalSetSize];
        }

        private Dictionary<MemoryType, UInt64> GetHeapAllocGroup()
        {
            return m_Data[(int)MemoryGroup.HeapAlloc];
        }

        private MemoryType NameToMemoryType(string name)
        {
            name = name.ToLower();
            if (name.Equals("native heap"))
                return MemoryType.NativeHeap;
            if (name.Equals("java heap"))
                return MemoryType.JavaHeap;
            if (name.Equals("code"))
                return MemoryType.Code;
            if (name.Equals("stack"))
                return MemoryType.Stack;
            if (name.Equals("graphics"))
                return MemoryType.Graphics;
            if (name.Equals("private other"))
                return MemoryType.PrivateOther;
            if (name.Equals("system"))
                return MemoryType.System;
            if (name.Equals("total"))
                return MemoryType.Total;
            if (name.Equals("total swap pss"))
                return MemoryType.TotalSwapPss;
            return MemoryType.Unknown;
        }

        internal AndroidMemoryStatistics()
        {
            foreach (var g in m_MemoryGroups)
            {
                m_Data[(int)g] = new Dictionary<MemoryType, UInt64>();
            }
        }

        internal void Clear()
        {
            foreach (var g in m_MemoryGroups)
            {
                m_Data[(int)g].Clear();
            }
        }

        internal void ParseAppSummary(string appSummary)
        {
            Dictionary<MemoryType, UInt64> data = GetPSSMemoryGroup();
            string pattern = @"([\w\s]+):\s+(\d+)";

            Regex r = new Regex(pattern, RegexOptions.IgnoreCase);
            MatchCollection matches = r.Matches(appSummary);
            UInt64 dummy;
            foreach (Match match in matches)
            {
                var name = match.Groups[1].Value.Trim().ToLower();
                var sizeInKBytes = UInt64.Parse(match.Groups[2].Value);
                MemoryType type = NameToMemoryType(name);
                if (type != MemoryType.Unknown)
                    data[type] = sizeInKBytes * kOneKiloByte;
            }

            if (!data.TryGetValue(MemoryType.NativeHeap, out dummy))
            {
                throw new Exception("Failed to find native heap size in\n" + appSummary);
            }
        }

        /// <summary>
        /// Some number values are in format like this 3(6)
        /// We need to convert those to 3
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string FixNumberValue(string value)
        {
            int index = value.IndexOf("(");
            return index == -1 ? value : value.Substring(0, index);
        }

        internal void ParseHeapInformation(string heapInformation)
        {
            var postFix = @"\s+\S+\s+\S+\s+\S+\s+\S+\s+(?<heapSize>\S+)\s+(?<heapAlloc>\S+)\s+\S+";

            Regex native = new Regex("Native Heap" + postFix, RegexOptions.IgnoreCase);
            Regex java = new Regex("Dalvik Heap" + postFix, RegexOptions.IgnoreCase);

            var regexes = new[] { native, java };
            var types = new[] { MemoryType.NativeHeap, MemoryType.JavaHeap };

            var totalHeapAlloc = (UInt64)0;
            var totalHeapSize = (UInt64)0;
            for (int i = 0; i < regexes.Length; i++)
            {
                var match = regexes[i].Match(heapInformation);
                if (match.Success)
                {
                    var value = UInt64.Parse(FixNumberValue(match.Groups["heapAlloc"].Value)) * kOneKiloByte;
                    SetValue(MemoryGroup.HeapAlloc, types[i], value);
                    totalHeapAlloc += value;

                    value = UInt64.Parse(FixNumberValue(match.Groups["heapSize"].Value)) * kOneKiloByte;
                    SetValue(MemoryGroup.HeapSize, types[i], value);
                    totalHeapSize += value;
                }
            }

            SetValue(MemoryGroup.HeapAlloc, MemoryType.Total, totalHeapAlloc);
            SetValue(MemoryGroup.HeapSize, MemoryType.Total, totalHeapSize);
        }

        internal UInt64 GetValue(MemoryGroup group, MemoryType type)
        {
            UInt64 value;
            if (m_Data[(int)group].TryGetValue(type, out value))
                return value;
            return 0;
        }

        internal void SetValue(MemoryGroup group, MemoryType type, UInt64 value)
        {
            m_Data[(int)group][type] = value;
        }

        /// <summary>
        /// Parses contents from command 'adb shell dumpsys meminfo package_name'
        /// </summary>
        /// <param name="contents"></param>
        /// <returns></returns>
        internal void Parse(string contents)
        {
            int appSummary = contents.IndexOf("App Summary");
            if (appSummary == -1)
                throw new Exception("Failed to find App Summary:\n" + contents);
            ParseHeapInformation(contents.Substring(0, appSummary));
            ParseAppSummary(contents.Substring(appSummary));
        }

        internal void SetPSSFakeData(UInt64 totalMemory, UInt64 nativeHeap)
        {
            SetValue(MemoryGroup.ProportionalSetSize, MemoryType.Total, totalMemory);
            SetValue(MemoryGroup.ProportionalSetSize, MemoryType.NativeHeap, nativeHeap);
        }

        internal void SetHeapAllocData(UInt64 totalMemory, UInt64 nativeHeap)
        {
            SetValue(MemoryGroup.HeapAlloc, MemoryType.Total, totalMemory);
            SetValue(MemoryGroup.HeapAlloc, MemoryType.NativeHeap, nativeHeap);
        }

        internal void SetHeapSizeData(UInt64 totalMemory, UInt64 nativeHeap)
        {
            SetValue(MemoryGroup.HeapSize, MemoryType.Total, totalMemory);
            SetValue(MemoryGroup.HeapSize, MemoryType.NativeHeap, nativeHeap);
        }
    }
}

#endif
