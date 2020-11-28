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
    internal enum MemoryViewerBehavior
    {
        Hidden,
        Auto,
        Manual
    }

    [Serializable]
    internal class AndroidLogcatMemoryViewerState
    {
        public float MemoryWindowHeight;
        public float MemoryWindowWidth;
        public bool[] MemoryTypeEnabled;
        public MemoryGroup MemoryGroup = MemoryGroup.HeapAlloc;
        public MemoryViewerBehavior Behavior = MemoryViewerBehavior.Auto;
    }

    internal class AndroidLogcatMemoryViewer
    {
        enum SplitterDragging
        {
            None,
            Horizontal,
            Vertical
        }

        class AndroidLogcatQueryMemoryInput : IAndroidLogcatTaskInput
        {
            internal ADB adb;
            internal int packageProcessId;
            internal string packageName;
            internal string deviceId;
        }

        class AndroidLogcatQueryMemoryResult : IAndroidLogcatTaskResult
        {
            internal int packageProcessId;
            internal string packageName;
            internal string contents;
            internal string deviceId;
        }

        private EditorWindow m_Parent;
        private AndroidLogcatRuntimeBase m_Runtime;
        private Material m_Material;

        const int kMaxEntries = 300;
        const UInt64 k16MB = 16 * 1000 * 1000;
        const float kMinMemoryWindowHeight = 255.0f;
        const float kMinMemoryWindowWidth = 170.0f;
        const float kMaxMemoryWindowWidth = 500.0f;
        private AndroidMemoryStatistics[] m_Entries = new AndroidMemoryStatistics[kMaxEntries];
        private AndroidMemoryStatistics m_LastAllocatedEntry = new AndroidMemoryStatistics();
        private int m_CurrentEntry = 0;
        private int m_EntryCount = 0;
        static readonly int kMemoryGroupCount = Enum.GetValues(typeof(MemoryGroup)).Length;
        private UInt64 m_UpperMemoryBoundry = 32 * 1000 * 1000;
        private int m_RequestsInQueue;
        private int m_SelectedEntry;
        private SplitterDragging m_SplitterDragging;
        private float m_SplitterStart;
        private float m_SplitterOldValue;

        private MemoryType[] m_OrderMemoryTypesPSS = new[]
        {
            MemoryType.NativeHeap,
            MemoryType.JavaHeap,
            MemoryType.Code,
            MemoryType.Stack,
            MemoryType.Graphics,
            MemoryType.PrivateOther,
            MemoryType.System
        };

        private MemoryType[] m_OrderMemoryTypesHeap = new[]
        {
            MemoryType.NativeHeap,
            MemoryType.JavaHeap,
        };


        private IAndroidLogcatDevice m_ExpectedDevice;
        private PackageInformation m_ExpectedPackageFromRequest;
        private AndroidLogcatMemoryViewerState m_State;


        private MemoryType[] GetOrderMemoryTypes()
        {
            return m_State.MemoryGroup == MemoryGroup.ProportionalSetSize ? m_OrderMemoryTypesPSS : m_OrderMemoryTypesHeap;
        }

        Dictionary<MemoryType, Color> m_MemoryTypeColors = new Dictionary<MemoryType, Color>();

        public AndroidLogcatMemoryViewer(EditorWindow parent, AndroidLogcatRuntimeBase runtime)
        {
            m_Parent = parent;
            m_Runtime = runtime;
            m_Material = (Material)EditorGUIUtility.LoadRequired("SceneView/HandleLines.mat");
            m_State = m_Runtime.ProjectSettings.MemoryViewerState;

            for (int i = 0; i < kMaxEntries; i++)
                m_Entries[i] = new AndroidMemoryStatistics();

            m_RequestsInQueue = 0;

            /*
            // For Debugging purposes
            for (int i = 0; i < kMaxEntries; i++)
            {
                InjectFakeMemoryStatistics((int)(UnityEngine.Random.value * k16MB * 2.0f));
            }
            //**/

            m_SplitterStart = 0;
            m_SplitterDragging = SplitterDragging.None;

            m_MemoryTypeColors[MemoryType.NativeHeap] = Color.red;
            m_MemoryTypeColors[MemoryType.JavaHeap] = Color.yellow;
            m_MemoryTypeColors[MemoryType.Code] = Color.blue;
            m_MemoryTypeColors[MemoryType.Stack] = Color.cyan;
            m_MemoryTypeColors[MemoryType.Graphics] = Color.green;
            m_MemoryTypeColors[MemoryType.PrivateOther] = Color.grey;
            m_MemoryTypeColors[MemoryType.System] = Color.magenta;
            m_MemoryTypeColors[MemoryType.Total] = Color.white;
            ClearEntries();
            ValidateSettings();
        }

        /// <summary>
        /// Validate serialized settings here
        /// </summary>
        private void ValidateSettings()
        {
            var allMemoryTypes = (MemoryType[])Enum.GetValues(typeof(MemoryType));

            if (m_State.MemoryTypeEnabled == null || m_State.MemoryTypeEnabled.Length != allMemoryTypes.Length)
            {
                m_State.MemoryTypeEnabled = new bool[allMemoryTypes.Length];
                for (int i = 0; i < m_State.MemoryTypeEnabled.Length; i++)
                    m_State.MemoryTypeEnabled[i] = true;
            }

            if (m_State.MemoryWindowHeight < kMinMemoryWindowHeight)
                m_State.MemoryWindowHeight = 300.0f;
            m_State.MemoryWindowWidth = Mathf.Clamp(m_State.MemoryWindowWidth, kMinMemoryWindowWidth, kMaxMemoryWindowWidth);
        }

        internal void ClearEntries()
        {
            m_SelectedEntry = -1;
            m_EntryCount = 0;
            m_CurrentEntry = 0;

            m_UpperMemoryBoundry = 32 * 1000 * 1000;
            m_ExpectedPackageFromRequest = null;
            m_ExpectedDevice = null;
        }

        internal void SetExpectedDeviceAndPackage(IAndroidLogcatDevice device, PackageInformation package)
        {
            m_ExpectedDevice = device;
            m_ExpectedPackageFromRequest = package;
        }

        internal void QueueMemoryRequest(IAndroidLogcatDevice device, PackageInformation package)
        {
            m_ExpectedDevice = device;
            m_ExpectedPackageFromRequest = package;
            if (m_ExpectedPackageFromRequest == null || !m_ExpectedPackageFromRequest.IsAlive() || m_ExpectedDevice == null)
                return;
            // Don't make a memory request, if previous requests haven't finished yet
            // Otherwise async queue will grow bigger and bigger
            const int kMaxRequestsInQueue = 3;
            if (m_RequestsInQueue > kMaxRequestsInQueue)
                return;
            m_RequestsInQueue++;
            m_Runtime.Dispatcher.Schedule(
                new AndroidLogcatQueryMemoryInput()
                {
                    adb = ADB.GetInstance(),
                    packageProcessId = m_ExpectedPackageFromRequest.processId,
                    packageName = m_ExpectedPackageFromRequest.name,
                    deviceId = device.Id
                },
                QueryMemoryAsync,
                IntegrateQueryMemory,
                false);
        }

        internal static string UInt64ToSizeString(UInt64 value)
        {
            if (value < 0)
                return "unknown";
            if (value == 0)
                return "0 Bytes";
            float val = (float)value;
            string[] scale = new string[] { "TB", "GB", "MB", "KB", "Bytes" };
            int idx = scale.Length - 1;
            while (val > 1000.0f && idx >= 0)
            {
                val /= 1000f;
                idx--;
            }

            if (idx < 0)
                return "<error>";

            return string.Format("{0:#.##} {1}", val, scale[idx]);
        }

        private static IAndroidLogcatTaskResult QueryMemoryAsync(IAndroidLogcatTaskInput input)
        {
            var workInput = ((AndroidLogcatQueryMemoryInput)input);
            var adb = workInput.adb;

            if (adb == null)
                throw new NullReferenceException("ADB interface has to be valid");

            var cmd = "-s " + workInput.deviceId + " shell dumpsys meminfo " + workInput.packageName;
            AndroidLogcatInternalLog.Log("{0} {1}", adb.GetADBPath(), cmd);

            string outputMsg = string.Empty;
            try
            {
                outputMsg = adb.Run(new[] { cmd }, "Failed to query memory for " + workInput.packageName);
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log("Failed to query memory: \n" + ex.Message);
            }
            var result = new AndroidLogcatQueryMemoryResult();
            result.deviceId = workInput.deviceId;
            result.packageName = workInput.packageName;
            result.packageProcessId = workInput.packageProcessId;
            result.contents = outputMsg;
            //AndroidLogcatInternalLog.Log(outputMsg);

            return result;
        }

        private AndroidMemoryStatistics AllocateMemoryStatistics()
        {
            m_LastAllocatedEntry = m_Entries[m_CurrentEntry++];
            if (m_CurrentEntry >= kMaxEntries)
                m_CurrentEntry = 0;
            m_EntryCount = Math.Min(m_EntryCount + 1, kMaxEntries);

            if (m_SelectedEntry >= 0 && m_EntryCount == kMaxEntries)
                m_SelectedEntry--;
            return m_LastAllocatedEntry;
        }

        private void UpdateGeneralStats()
        {
            // Set the upper boundry depending on total memory from all groups
            foreach (var m in (MemoryGroup[])Enum.GetValues(typeof(MemoryGroup)))
            {
                UInt64 maxMemory = 0;
                for (int i = 0; i < m_EntryCount; i++)
                {
                    UInt64 localTotal = 0;
                    foreach (var t in GetOrderMemoryTypes())
                    {
                        if (!m_State.MemoryTypeEnabled[(int)t])
                            continue;
                        localTotal += m_Entries[ResolveEntryIndex(i)].GetValue(m_State.MemoryGroup, t);
                    }
                    maxMemory = Math.Max(maxMemory, localTotal);
                }
                // Keep boundry by 10% higher, so there would be visible room from the top of the window
                m_UpperMemoryBoundry = (UInt64)(1.1f * maxMemory);
            }
        }

        private void InjectFakeMemoryStatistics(UInt64 totalMemory)
        {
            var stats = AllocateMemoryStatistics();
            stats.SetPSSFakeData(totalMemory, totalMemory);
            stats.SetHeapAllocData(totalMemory, totalMemory);
            stats.SetHeapSizeData(totalMemory, totalMemory);
            UpdateGeneralStats();
        }

        private void IntegrateQueryMemory(IAndroidLogcatTaskResult result)
        {
            m_RequestsInQueue--;
            if (m_RequestsInQueue < 0)
            {
                m_RequestsInQueue = 0;
                throw new Exception("Receiving more memory results than requested ?");
            }
            var memoryResult = (AndroidLogcatQueryMemoryResult)result;

            // When selecting a new package, there might be still few requests for other packages running on other threads
            // Ignore those
            if (m_ExpectedPackageFromRequest == null || m_ExpectedDevice == null)
                return;

            if (memoryResult.packageProcessId != m_ExpectedPackageFromRequest.processId ||
                memoryResult.deviceId != m_ExpectedDevice.Id ||
                string.IsNullOrEmpty(memoryResult.contents))
                return;

            if (memoryResult.contents.Contains("No process found for:"))
            {
                m_ExpectedPackageFromRequest.SetExited();
                m_Parent.Repaint();
                return;
            }

            var stats = AllocateMemoryStatistics();
            try
            {
                stats.Parse(memoryResult.contents);
            }
            catch (Exception ex)
            {
                stats.Clear();
                AndroidLogcatInternalLog.Log(ex.Message);
            }
            UpdateGeneralStats();

            m_Parent.Repaint();
        }

        private float GetEntryWidth(Rect windowSize)
        {
            return windowSize.width / (kMaxEntries - 1);
        }

        private int ResolveEntryIndex(int entry)
        {
            return (int)Mathf.Repeat(entry + m_CurrentEntry - m_EntryCount, kMaxEntries);
        }

        private Color GetMemoryColor(MemoryType type)
        {
            Color color;
            if (m_MemoryTypeColors.TryGetValue(type, out color))
                return color;
            throw new NotImplementedException(type.ToString());
        }

        private void DoMemoryToggle(MemoryType type)
        {
            GUILayout.Space(10);
            GUILayout.BeginHorizontal();
            GUILayout.Space(10);
            Color oldColor = GUI.backgroundColor;
            var memory = m_ExpectedPackageFromRequest == null ? "0" : UInt64ToSizeString(m_LastAllocatedEntry.GetValue(m_State.MemoryGroup, type));
            var name = String.Format("{0} ({1})", type, memory);
            if (type == MemoryType.Total)
            {
                GUI.backgroundColor = Color.white;
                GUILayout.Toggle(true, name, AndroidLogcatStyles.kSeriesLabel);
            }
            else
            {
                var enabled = m_State.MemoryTypeEnabled[(int)type];
                GUI.backgroundColor = enabled ? GetMemoryColor(type) : Color.black;
                EditorGUI.BeginChangeCheck();
                m_State.MemoryTypeEnabled[(int)type] = GUILayout.Toggle(enabled, name, AndroidLogcatStyles.kSeriesLabel);
                if (EditorGUI.EndChangeCheck())
                    UpdateGeneralStats();
            }
            GUI.backgroundColor = oldColor;
            GUILayout.EndHorizontal();
        }

        private void ClearSplitterOperation()
        {
            m_SplitterDragging = SplitterDragging.None;
            m_SplitterStart = 0.0f;
            m_SplitterOldValue = 0.0f;
        }

        private bool DoSplitter(Rect verticalSplitter, Rect horizontalSplitter)
        {
            EditorGUIUtility.AddCursorRect(verticalSplitter, MouseCursor.ResizeVertical);
            EditorGUIUtility.AddCursorRect(horizontalSplitter, MouseCursor.ResizeHorizontal);
            var e = Event.current;
            switch (e.type)
            {
                case EventType.MouseDown:
                    if (verticalSplitter.Contains(e.mousePosition))
                    {
                        m_SplitterDragging = SplitterDragging.Vertical;
                        m_SplitterOldValue = m_State.MemoryWindowHeight;
                        m_SplitterStart = e.mousePosition.y;
                        e.Use();
                        return true;
                    }

                    if (horizontalSplitter.Contains(e.mousePosition))
                    {
                        m_SplitterDragging = SplitterDragging.Horizontal;
                        m_SplitterOldValue = m_State.MemoryWindowWidth;
                        m_SplitterStart = e.mousePosition.x;
                        e.Use();
                        return true;
                    }
                    break;
                case EventType.MouseDrag:
                case EventType.MouseUp:
                    switch (m_SplitterDragging)
                    {
                        case SplitterDragging.Vertical:
                            m_State.MemoryWindowHeight = Math.Max(m_SplitterOldValue + m_SplitterStart - e.mousePosition.y, kMinMemoryWindowHeight);
                            if (e.type == EventType.MouseUp)
                                ClearSplitterOperation();
                            e.Use();
                            return true;
                        case SplitterDragging.Horizontal:
                            m_State.MemoryWindowWidth = Mathf.Clamp(m_SplitterOldValue + e.mousePosition.x - m_SplitterStart, kMinMemoryWindowWidth, kMaxMemoryWindowWidth);
                            if (e.type == EventType.MouseUp)
                                ClearSplitterOperation();
                            e.Use();
                            return true;
                    }
                    break;
            }

            return false;
        }

        internal void DoGUI()
        {
            if (m_State.Behavior == MemoryViewerBehavior.Hidden)
                return;

            var splitterRectVertical = GUILayoutUtility.GetRect(GUIContent.none, GUIStyle.none, GUILayout.ExpandWidth(true), GUILayout.Height(5));
            var splitterRectHorizontal = new Rect(m_State.MemoryWindowWidth, splitterRectVertical.y, 5, m_State.MemoryWindowHeight);
            DoSplitter(splitterRectVertical, splitterRectHorizontal);
            GUILayout.BeginHorizontal();

            GUILayout.BeginVertical(GUILayout.Width(m_State.MemoryWindowWidth), GUILayout.Height(m_State.MemoryWindowHeight));

            GUILayout.Space(10);
            GUILayout.BeginHorizontal();
            GUILayout.Label("Group:");
            EditorGUI.BeginChangeCheck();
            m_State.MemoryGroup = (MemoryGroup)EditorGUILayout.EnumPopup(m_State.MemoryGroup);
            if (EditorGUI.EndChangeCheck())
                UpdateGeneralStats();
            GUILayout.EndHorizontal();

            foreach (var m in GetOrderMemoryTypes())
            {
                DoMemoryToggle(m);
            }

            DoMemoryToggle(MemoryType.Total);

            if (m_State.Behavior == MemoryViewerBehavior.Manual)
            {
                GUILayout.Space(10);
                if (GUILayout.Button("Capture", EditorStyles.miniButton))
                    QueueMemoryRequest(m_ExpectedDevice, m_ExpectedPackageFromRequest);
            }

            if (Unsupported.IsDeveloperMode())
                DoDebuggingGUI();

            GUILayout.EndVertical();
            var rc = GUILayoutUtility.GetLastRect();


            GUILayout.BeginVertical();
            // Note: GUILayoutUtility.GetRect must be called for Layout event always
            var size = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.internalLogStyle, GUILayout.Height(m_State.MemoryWindowHeight));

            size.height -= 4;


            if (m_EntryCount > 0)
                DoEntriesGUI(size);

            DoGuidelinesGUI(size, m_UpperMemoryBoundry);

            if (m_EntryCount > 0)
                DoSelectedStatsGUI(size);

            GUI.Box(new Rect(rc.x + 4, size.y, rc.width - 4, size.height + 1), GUIContent.none, EditorStyles.helpBox);
            GUI.Box(new Rect(size.x, size.y, size.width + 1, size.height + 1), GUIContent.none, EditorStyles.helpBox);

            if (m_ExpectedPackageFromRequest == null)
                EditorGUI.HelpBox(size, "Select a package", MessageType.Info);

            GUILayout.EndVertical();

            GUILayout.EndHorizontal();
        }

        private UInt64 AggregateMemorySize(AndroidMemoryStatistics stats, MemoryType type)
        {
            UInt64 total = 0;
            MemoryType[] types = GetOrderMemoryTypes();
            for (int i = types.Length - 1; i >= 0; i--)
            {
                if (types[i] == type)
                    return total;
                if (!m_State.MemoryTypeEnabled[i])
                    continue;
                total += stats.GetValue(m_State.MemoryGroup, types[i]);
            }

            throw new Exception("Unhandled memory type: " + type);
        }

        private void DoGuidelinesGUI(Rect windowSize, UInt64 totalMemorySize)
        {
            if (Event.current.type != EventType.Repaint)
                return;

            m_Material.SetPass(0);
            var percentages = new[] { 0.4f, 0.8f };
            GL.Begin(GL.LINES);
            foreach (var p in percentages)
            {
                float y = windowSize.y + windowSize.height * (1.0f - p);
                GL.Color(Color.gray);
                GL.Vertex3(windowSize.x, y, 0);
                GL.Vertex3(windowSize.x + windowSize.width, y, 0);
            }
            GL.End();

            if (m_ExpectedPackageFromRequest == null)
                return;

            foreach (var p in percentages)
            {
                float y = windowSize.y + windowSize.height * (1.0f - p);
                var title = UInt64ToSizeString((UInt64)(totalMemorySize * p));
                AndroidLogcatStyles.infoStyle.Draw(new Rect(windowSize.x, y, 100, 20), new GUIContent(title), 0);
            }
        }

        private void DoEntriesGUI(Rect windowSize)
        {
            if (Event.current.type != EventType.Repaint)
                return;

            m_Material.SetPass(0);

            // Triangle strip
            // 0  2
            // | /|
            // |/ |
            // 1  3
            var width = GetEntryWidth(windowSize);
            var multiplier = windowSize.height / m_UpperMemoryBoundry;
            var t = windowSize.y;
            var b = windowSize.height + windowSize.y;
            var xOffset = windowSize.x + windowSize.width - (m_EntryCount - 1) * width;

            foreach (var m in GetOrderMemoryTypes())
            {
                if (!m_State.MemoryTypeEnabled[(int)m])
                    continue;
                GL.Begin(GL.TRIANGLE_STRIP);
                GL.Color(GetMemoryColor(m));

                for (int i = 0; i < m_EntryCount; i++)
                {
                    var idx = ResolveEntryIndex(i);
                    var agr = AggregateMemorySize(m_Entries[idx], m);
                    var val = m_Entries[idx].GetValue(m_State.MemoryGroup, m);
                    var x = xOffset + i * width;
                    var y1 = b - multiplier * (val + agr);
                    var y2 = b - multiplier * agr;
                    GL.Vertex3(x, y1, 0);
                    GL.Vertex3(x, y2, 0);
                }
                GL.End();
            }
        }

        private void DoSelectedStatsGUI(Rect windowSize)
        {
            var e = Event.current;
            if (e.type == EventType.MouseDown && windowSize.Contains(e.mousePosition))
            {
                float wd = GetEntryWidth(windowSize);
                m_SelectedEntry = (int)((e.mousePosition.x - windowSize.x + wd * 0.5f) / wd);
                // Correct entry for cases where we don't have enough entries to fill the full array
                m_SelectedEntry += m_EntryCount - kMaxEntries;
                m_Parent.Repaint();
            }

            if (m_SelectedEntry < 0)
                return;
            var width = GetEntryWidth(windowSize);
            var x = windowSize.x + windowSize.width - (m_EntryCount - 1) * width + m_SelectedEntry * width;
            var t = windowSize.y;
            var b = windowSize.height + windowSize.y;
            if (e.type == EventType.Repaint)
            {
                m_Material.SetPass(0);
                GL.Begin(GL.LINES);
                GL.Color(Color.white);
                GL.Vertex3(x, t, 0);
                GL.Vertex3(x, b, 0);
                GL.End();
            }

            var idx = ResolveEntryIndex(m_SelectedEntry);
            var info = new StringBuilder();

            int enabledCount = 0;
            foreach (var m in GetOrderMemoryTypes())
            {
                if (!m_State.MemoryTypeEnabled[(int)m])
                    continue;
                info.AppendLine(m.ToString() + " : " + UInt64ToSizeString(m_Entries[idx].GetValue(m_State.MemoryGroup, m)));
                enabledCount++;
            }

            info.AppendLine("Total: " + UInt64ToSizeString(m_Entries[idx].GetValue(m_State.MemoryGroup, MemoryType.Total)));

            const float kInfoWidth = 150;
            var infoX = x + 5;
            if (infoX + kInfoWidth > windowSize.x + windowSize.width)
                infoX -= kInfoWidth + 10;
            var rc = new Rect(infoX, t + 10, kInfoWidth, 19 * enabledCount + 30);
            GUI.Box(rc, GUIContent.none, GUI.skin.window);
            GUI.Label(rc, info.ToString());
        }

        void DoDebuggingGUI()
        {
            GUILayout.Space(20);
            GUILayout.Label("Developer Options", EditorStyles.boldLabel);
            const UInt64 kOneKiloByte = 1000;
            const UInt64 kOneMegaByte = kOneKiloByte * kOneKiloByte;
            const UInt64 kOneGigaByte = kOneKiloByte * kOneMegaByte;
            if (GUILayout.Button("Add 400MB", EditorStyles.miniButton))
                InjectFakeMemoryStatistics(400 * kOneMegaByte);
            if (GUILayout.Button("Add 2GB", EditorStyles.miniButton))
                InjectFakeMemoryStatistics(2 * kOneGigaByte);
            if (GUILayout.Button("Add 1000GB", EditorStyles.miniButton))
                InjectFakeMemoryStatistics(1000 * kOneGigaByte);
        }
    }
}

#endif
