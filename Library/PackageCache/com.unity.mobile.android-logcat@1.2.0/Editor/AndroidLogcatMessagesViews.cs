#if PLATFORM_ANDROID
using System;
using System.IO;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using UnityEditor;

namespace Unity.Android.Logcat
{
    internal partial class AndroidLogcatConsoleWindow
    {
        internal enum Column
        {
            Icon,
            Time,
            ProcessId,
            ThreadId,
            Priority,
            Tag,
            Message
        }

        private List<int> m_SelectedIndices = new List<int>();
        private Vector2 m_ScrollPosition = Vector2.zero;
        private float m_MaxLogEntryWidth = 0.0f;

        private bool m_Autoscroll = true;
        private float doubleClickStart = -1;

        private ColumnData[] Columns
        {
            get
            {
                return m_Runtime.Settings.ColumnData;
            }
        }

        private bool DoSplitter(ColumnData data, Rect splitterRect)
        {
            const float kSplitterWidth = 3.0f;
            splitterRect.x = splitterRect.x + splitterRect.width - kSplitterWidth * 0.5f;
            splitterRect.width = kSplitterWidth;


            EditorGUIUtility.AddCursorRect(splitterRect, MouseCursor.ResizeHorizontal);
            var e = Event.current;
            switch (e.type)
            {
                case EventType.MouseDown:
                    if (splitterRect.Contains(e.mousePosition))
                    {
                        data.splitterDragging = true;
                        data.splitterDragStartMouseValue = e.mousePosition.x;
                        data.splitterDragStartWidthValue = data.width;
                        e.Use();
                        return true;
                    }
                    break;
                case EventType.MouseDrag:
                case EventType.MouseUp:
                    if (data.splitterDragging)
                    {
                        data.width = Mathf.Max(20.0f, data.splitterDragStartWidthValue + e.mousePosition.x - data.splitterDragStartMouseValue);

                        if (e.type == EventType.MouseUp)
                        {
                            data.splitterDragging = false;
                        }
                        e.Use();
                        return true;
                    }
                    break;
            }

            return false;
        }

        private bool ShowColumn(Column column)
        {
            if (column == Column.Icon)
            {
                return m_Runtime.Settings.MessageFontSize > 11 && Columns[(int)column].enabled;
            }

            return Columns[(int)column].enabled;
        }

        private bool DoGUIHeader()
        {
            bool requestRepaint = false;
            var fullHeaderRect = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.columnHeader, GUILayout.ExpandWidth(true));
            bool headerDrawn = false;
            bool lastHeaderDrawn = false;
            var offset = 0.0f;
            foreach (var c in (Column[])Enum.GetValues(typeof(Column)))
            {
                if (!ShowColumn(c))
                    continue;
                var d = Columns[(int)c];

                d.itemSize = new Rect(offset, fullHeaderRect.y, d.width, fullHeaderRect.height);
                offset += d.width;

                if (d.width > 0.0f)
                {
                    var buttonRect = d.itemSize;
                    buttonRect.x -= m_ScrollPosition.x;

                    switch ((Column)c)
                    {
                        case Column.Priority:
                            if (GUI.Button(buttonRect, d.content, AndroidLogcatStyles.columnHeader))
                            {
                                var priorities = (AndroidLogcat.Priority[])Enum.GetValues(typeof(AndroidLogcat.Priority));
                                EditorUtility.DisplayCustomMenu(new Rect(Event.current.mousePosition, Vector2.zero), priorities.Select(m => new GUIContent(m.ToString())).ToArray(), (int)m_Runtime.ProjectSettings.SelectedPriority, PrioritySelection, null);
                            }
                            break;
                        case Column.Tag:
                            if (GUI.Button(buttonRect, d.content, AndroidLogcatStyles.columnHeader))
                            {
                                m_Runtime.ProjectSettings.Tags.DoGUI(new Rect(Event.current.mousePosition, Vector2.zero), buttonRect);
                            }
                            break;
                        default:
                            GUI.Label(buttonRect, d.content, AndroidLogcatStyles.columnHeader);
                            break;
                    }

                    requestRepaint |= DoSplitter(d, buttonRect);
                }
                else
                {
                    var buttonRect = d.itemSize;
                    buttonRect.x -= m_ScrollPosition.x;
                    buttonRect.width = fullHeaderRect.width - offset + m_ScrollPosition.x;

                    GUI.Label(buttonRect, d.content, AndroidLogcatStyles.columnHeader);
                    // For last entry have a really big width, so all the message can fit
                    d.itemSize.width = 10000.0f;
                    lastHeaderDrawn = true;
                }

                if (headerDrawn)
                {
                    // Don't allow splitter to make item small than 4px
                    // No need to do it for first visible item
                    d.itemSize.x = Mathf.Max(4.0f, d.itemSize.x);
                }
                headerDrawn = true;
            }

            if (!headerDrawn)
            {
                // If no header drawn, draw a empty label to the full header rect.
                GUI.Label(fullHeaderRect, GUIContent.none, AndroidLogcatStyles.columnHeader);
            }
            else if (!lastHeaderDrawn)
            {
                // If no last header drawn, draw an empty label to the remained header rect.
                float x = offset - m_ScrollPosition.x;
                var buttonRect = new Rect(x, fullHeaderRect.y, fullHeaderRect.width - x, fullHeaderRect.height);
                GUI.Label(buttonRect, GUIContent.none, AndroidLogcatStyles.columnHeader);
            }

            DoMouseEventsForHeaderToolbar(fullHeaderRect);
            return requestRepaint;
        }

        private void MenuSelectionColumns(object userData, string[] options, int selected)
        {
            if (options[selected] == "Clear All")
            {
                foreach (var c in Columns)
                    c.enabled = false;
            }
            else if (options[selected] == "Select All")
            {
                foreach (var c in Columns)
                    c.enabled = true;
            }
            else if (selected < Columns.Length)
                Columns[selected].enabled = !Columns[selected].enabled;
        }

        private void PrioritySelection(object userData, string[] options, int selected)
        {
            SetSelectedPriority((AndroidLogcat.Priority)selected);
        }

        private void DoMouseEventsForHeaderToolbar(Rect headerRect)
        {
            var e = Event.current;
            if (e.type == EventType.MouseDown && headerRect.Contains(e.mousePosition))
            {
                switch (e.button)
                {
                    case 1:
                        var menuTexts = new List<string>();
                        var menuSelected = new List<int>();
                        for (int i = 0; i < Columns.Length; i++)
                        {
                            menuTexts.Add(((Column)i).ToString());
                            if (Columns[i].enabled)
                                menuSelected.Add(i);
                        }

                        menuTexts.Add("");
                        menuTexts.Add("Clear All");
                        menuTexts.Add("Select All");
                        e.Use();

                        var enabled = Enumerable.Repeat(true, menuTexts.Count).ToArray();
                        var separator = new bool[menuTexts.Count];
                        EditorUtility.DisplayCustomMenuWithSeparators(new Rect(e.mousePosition.x, e.mousePosition.y, 0, 0),
                            menuTexts.ToArray(),
                            enabled,
                            separator,
                            menuSelected.ToArray(),
                            MenuSelectionColumns,
                            null);
                        break;
                }
            }
        }

        private void DoIconLogEntryItem(Rect fullView, int index, Column column, string value, GUIStyle style, Vector2 iconSize)
        {
            if (!ShowColumn(column))
                return;
            var itemRect = Columns[(uint)column].itemSize;
            var entryHeight = AndroidLogcatStyles.kLogEntryFixedHeight;
            var rc = new Rect(itemRect.x + (itemRect.width - iconSize.x) * 0.5f, fullView.y + entryHeight * index + (entryHeight - iconSize.y) * 0.5f, 0, 0);
            style.Draw(rc, new GUIContent(value), 0);
        }

        private void DoLogEntryItem(Rect fullView, int index, Column column, string value, GUIStyle style)
        {
            if (!ShowColumn(column))
                return;
            const float kMessageMargin = 5;
            var itemRect = Columns[(uint)column].itemSize;
            var rc = new Rect(itemRect.x + kMessageMargin, fullView.y + AndroidLogcatStyles.kLogEntryFixedHeight * index, itemRect.width - kMessageMargin, itemRect.height);
            style.Draw(rc, new GUIContent(value), 0);
        }

        private GUIStyle GetIconStyle(AndroidLogcat.Priority priority)
        {
            switch (priority)
            {
                case AndroidLogcat.Priority.Warn:
                    return AndroidLogcatStyles.warningSmallStyle;
                case AndroidLogcat.Priority.Error:
                case AndroidLogcat.Priority.Fatal:
                    return AndroidLogcatStyles.errorSmallStyle;
                default:
                    return AndroidLogcatStyles.infoSmallStyle;
            }
        }

        private bool DoGUIEntries()
        {
            bool requestRepaint = false;
            var e = Event.current;

            var visibleWindowRect = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.priorityDefaultStyle, GUILayout.ExpandWidth(true), GUILayout.ExpandHeight(true));
            var totalWindowRect = visibleWindowRect;
            var maxVisibleItems = (int)(visibleWindowRect.height / AndroidLogcatStyles.kLogEntryFixedHeight);
            // Extra message count ensures that there's an empty space below when we scrolling all the way down
            // This way it's easier to see that there's no more messages
            const int kExtraMessageCount = 5;
            totalWindowRect.height = AndroidLogcatStyles.kLogEntryFixedHeight * (m_LogEntries.Count + kExtraMessageCount);
            totalWindowRect.width = Mathf.Max(totalWindowRect.width, m_MaxLogEntryWidth);

            var controlId = GUIUtility.GetControlID(FocusType.Keyboard);

            if (m_Autoscroll)
                m_ScrollPosition.y = totalWindowRect.height;

            EditorGUI.BeginChangeCheck();
            m_ScrollPosition = GUI.BeginScrollView(visibleWindowRect, m_ScrollPosition, totalWindowRect, true, false);
            int startItem = (int)(m_ScrollPosition.y / totalWindowRect.height * (kExtraMessageCount + m_LogEntries.Count));

            // Check if we need to enable autoscrolling
            if (EditorGUI.EndChangeCheck() || (e.type == EventType.ScrollWheel && e.delta.y > 0.0f))
                m_Autoscroll = startItem + maxVisibleItems - kExtraMessageCount >= m_LogEntries.Count;
            else if (e.type == EventType.ScrollWheel && e.delta.y < 0.0f)
                m_Autoscroll = false;

            if (e.type == EventType.Repaint)
            {
                // Max Log Entry width is used for calculating horizontal scrollbar
                m_MaxLogEntryWidth = 0.0f;
            }

            // Only draw items which can be visible on the screen
            // There can be thousands of log entries, drawing them all would kill performance
            for (int i = startItem; i - startItem < maxVisibleItems && i < m_LogEntries.Count; i++)
            {
                bool selected = m_SelectedIndices.Contains(i);
                var selectionRect = new Rect(visibleWindowRect.x, visibleWindowRect.y + AndroidLogcatStyles.kLogEntryFixedHeight * i, totalWindowRect.width, AndroidLogcatStyles.kLogEntryFixedHeight);

                if (e.type == EventType.Repaint)
                {
                    var le = m_LogEntries[i];
                    if (selected)
                        AndroidLogcatStyles.background.Draw(selectionRect, false, false, true, false);
                    else
                    {
                        if (i % 2 == 0)
                            AndroidLogcatStyles.backgroundEven.Draw(selectionRect, false, false, false, false);
                        else
                            AndroidLogcatStyles.backgroundOdd.Draw(selectionRect, false, false, false, false);
                    }
                    var style = AndroidLogcatStyles.priorityStyles[(int)le.priority];
                    DoIconLogEntryItem(visibleWindowRect, i, Column.Icon, "", GetIconStyle(le.priority), AndroidLogcatStyles.kSmallIconSize);
                    DoLogEntryItem(visibleWindowRect, i, Column.Time, le.dateTime.ToString(AndroidLogcat.LogEntry.s_TimeFormat), style);
                    DoLogEntryItem(visibleWindowRect, i, Column.ProcessId, le.processId.ToString(), style);
                    DoLogEntryItem(visibleWindowRect, i, Column.ThreadId, le.threadId.ToString(), style);
                    DoLogEntryItem(visibleWindowRect, i, Column.Priority, le.priority.ToString(), style);
                    DoLogEntryItem(visibleWindowRect, i, Column.Tag, le.tag.ToString(), style);
                    DoLogEntryItem(visibleWindowRect, i, Column.Message, le.message, style);

                    m_MaxLogEntryWidth = Mathf.Max(m_MaxLogEntryWidth,
                        AndroidLogcatStyles.priorityDefaultStyle.CalcSize(new GUIContent(le.message)).x + Columns[(int)Column.Message].itemSize.x);
                }
                else
                {
                    requestRepaint |= DoMouseEventsForLogEntry(selectionRect, i, selected, controlId);
                }
            }

            requestRepaint |= DoKeyEvents();

            GUI.EndScrollView();

            Rect rc = GUILayoutUtility.GetLastRect();
            // Decrement horizontal scrollbar height
            rc.height -= 15.0f;
            DoColumnBorders(rc, Color.black, 1);

            return requestRepaint;
        }

        private void DoColumnBorders(Rect visibleWindowRect, Color borderColor, float borderWidth)
        {
            if (Event.current.type != EventType.Repaint)
                return;
            var orgColor = GUI.color;
            GUI.color = borderColor;
            for (int i = 0; i < Enum.GetValues(typeof(Column)).Length; i++)
            {
                if (!ShowColumn((Column)i))
                    continue;
                var itemRect = Columns[i].itemSize;
                var rc = new Rect(itemRect.x + itemRect.width - m_ScrollPosition.x, visibleWindowRect.y, borderWidth, visibleWindowRect.height);
                GUI.DrawTexture(rc, EditorGUIUtility.whiteTexture);
            }

            GUI.color = orgColor;
        }

        private static bool HasCtrlOrCmdModifier(Event e)
        {
            return (e.modifiers & (Application.platform == RuntimePlatform.OSXEditor ? EventModifiers.Command : EventModifiers.Control)) != 0;
        }

        private void DoMouseSelection(Event e, int logEntryIndex, bool isLogEntrySelected, int keyboardControlId)
        {
            if (HasCtrlOrCmdModifier(e))
            {
                if (m_SelectedIndices.Contains(logEntryIndex))
                    m_SelectedIndices.Remove(logEntryIndex);
                else
                    m_SelectedIndices.Add(logEntryIndex);
            }
            else if ((e.modifiers & EventModifiers.Shift) != 0)
            {
                if (m_SelectedIndices.Count == 0)
                {
                    m_SelectedIndices.Add(logEntryIndex);
                }
                else
                {
                    int minValue = logEntryIndex;
                    int maxValue = logEntryIndex;
                    foreach (var si in m_SelectedIndices)
                    {
                        if (si > maxValue)
                            maxValue = si;
                        else if (si < minValue)
                            minValue = si;
                    }

                    for (int si = minValue; si <= maxValue; si++)
                    {
                        if (m_SelectedIndices.Contains(si))
                            continue;
                        m_SelectedIndices.Add(si);
                    }
                }
            }
            else
            {
                if (isLogEntrySelected && m_SelectedIndices.Count == 1)
                {
                    if ((Time.realtimeSinceStartup - doubleClickStart) < 0.3f)
                        TryToOpenFileFromLogEntry(m_LogEntries[logEntryIndex]);
                    doubleClickStart = -1;
                }
                else
                {
                    // Curious behavior with right click. In Unity if you right click on already selected item which is a part of selection list, it doesn't deselect other items
                    // But if you right click on unselected item, the selection list will be cleared
                    if (e.button == 0 ||
                        (e.button == 1 && !m_SelectedIndices.Contains(logEntryIndex)))
                    {
                        m_SelectedIndices.Clear();
                        m_SelectedIndices.Add(logEntryIndex);
                    }
                    doubleClickStart = Time.realtimeSinceStartup;
                }
            }

            m_SelectedIndices.Sort();
            GUIUtility.keyboardControl = keyboardControlId;
        }

        void DoContextMenu(Event e)
        {
            var entries = new List<AndroidLogcat.LogEntry>();
            foreach (var si in m_SelectedIndices)
            {
                if (si > m_LogEntries.Count - 1)
                    continue;
                entries.Add(m_LogEntries[si]);
            }
            var menuItems = new List<string>();
            menuItems.AddRange(new[] { "Copy", "Select All", "", "Save Selection..." });

            if (entries.Count > 0)
            {
                menuItems.Add("");
                menuItems.Add("Add tag '" + entries[0].tag + "'");
                menuItems.Add("Remove tag '" + entries[0].tag + "'");
                menuItems.Add("");
                menuItems.Add("Filter by process id '" + entries[0].processId + "'");
            }

            var enabled = Enumerable.Repeat(true, menuItems.Count).ToArray();
            var separator = new bool[menuItems.Count];
            EditorUtility.DisplayCustomMenuWithSeparators(new Rect(e.mousePosition.x, e.mousePosition.y, 0, 0),
                menuItems.ToArray(),
                enabled,
                separator,
                null,
                MenuSelection,
                entries.ToArray());
        }

        private bool DoMouseEventsForLogEntry(Rect logEntryRect, int logEntryIndex, bool isLogEntrySelected, int keyboardControlId)
        {
            bool requestRepaint = false;
            var e = Event.current;
            if (e.type == EventType.MouseDown && logEntryRect.Contains(e.mousePosition))
            {
                // Selection occurs both with Left Click & and Right click, this happens in all Unity windows.
                if (e.button == 0 || e.button == 1)
                {
                    DoMouseSelection(e, logEntryIndex, isLogEntrySelected, keyboardControlId);

                    requestRepaint = true;
                    e.Use();
                }
            }

            if (e.type == EventType.MouseUp && logEntryRect.Contains(e.mousePosition))
            {
                if (e.button == 1)
                {
                    DoContextMenu(e);
                    requestRepaint = true;
                    e.Use();
                }
            }

            return requestRepaint;
        }

        private bool DoKeyEvents()
        {
            var requestRepaint = false;
            var e = Event.current;
            if (e.type == EventType.KeyDown)
            {
                bool hasCtrlOrCmd = HasCtrlOrCmdModifier(e);
                switch (e.keyCode)
                {
                    case KeyCode.A:
                        if (hasCtrlOrCmd)
                        {
                            SelectAll();
                            e.Use();
                            requestRepaint = true;
                        }
                        break;
                    case KeyCode.C:
                        if (hasCtrlOrCmd)
                        {
                            var entries = new List<AndroidLogcat.LogEntry>(m_SelectedIndices.Count);
                            foreach (var si in m_SelectedIndices)
                            {
                                if (si >= m_LogEntries.Count)
                                    continue;
                                entries.Add(m_LogEntries[si]);
                            }
                            EditorGUIUtility.systemCopyBuffer = LogEntriesToString(entries.ToArray());
                            e.Use();
                        }
                        break;
                    case KeyCode.S:
                        if (hasCtrlOrCmd)
                        {
                            var logEntries = new List<AndroidLogcat.LogEntry>();
                            foreach (var si in m_SelectedIndices)
                            {
                                if (si > m_LogEntries.Count - 1)
                                    continue;
                                logEntries.Add(m_LogEntries[si]);
                            }
                            SaveToFile(logEntries.ToArray());
                            e.Use();
                        }
                        break;
                    default:
                        break;
                }
            }

            return requestRepaint;
        }

        public bool DoMessageView()
        {
            return DoGUIHeader() | DoGUIEntries();
        }

        private void SelectAll()
        {
            m_SelectedIndices.Clear();
            for (int si = 0; si < m_LogEntries.Count; si++)
                m_SelectedIndices.Add(si);
        }

        private void SaveToFile(AndroidLogcat.LogEntry[] logEntries)
        {
            var contents = LogEntriesToString(logEntries);
            var filePath = EditorUtility.SaveFilePanel("Save selected logs", "", PlayerSettings.applicationIdentifier + "-logcat", "txt");
            if (!string.IsNullOrEmpty(filePath))
                File.WriteAllText(filePath, contents);
        }

        private string LogEntriesToString(AndroidLogcat.LogEntry[] entries)
        {
            var contents = new StringBuilder();
            foreach (var l in entries)
            {
                var entry = string.Empty;
                for (int i = 0; i < Columns.Length; i++)
                {
                    if (!ShowColumn((Column)i))
                        continue;
                    if (entry.Length > 0)
                        entry += " ";
                    switch ((Column)i)
                    {
                        case Column.Time: entry += l.dateTime.ToString(AndroidLogcat.LogEntry.s_TimeFormat); break;
                        case Column.ProcessId: entry += l.processId; break;
                        case Column.ThreadId: entry += l.threadId; break;
                        case Column.Priority: entry += l.priority; break;
                        case Column.Tag: entry += l.tag; break;
                        case Column.Message: entry += l.message; break;
                    }
                }
                contents.AppendLine(entry);
            }

            return contents.ToString();
        }

        private void MenuSelection(object userData, string[] options, int selected)
        {
            switch (selected)
            {
                // Copy
                case 0:
                    EditorGUIUtility.systemCopyBuffer = LogEntriesToString((AndroidLogcat.LogEntry[])userData);
                    break;
                // Select All
                case 1:
                    SelectAll();
                    break;
                // Save to File
                case 3:
                    SaveToFile((AndroidLogcat.LogEntry[])userData);
                    break;
                // Add tag
                case 5:
                    AddTag(((AndroidLogcat.LogEntry[])userData)[0].tag);
                    break;
                // Remove tag
                case 6:
                    RemoveTag(((AndroidLogcat.LogEntry[])userData)[0].tag);
                    break;
                // Filter by process id
                case 8:
                    FilterByProcessId(((AndroidLogcat.LogEntry[])userData)[0].processId);
                    break;
            }
        }

        private void TryToOpenFileFromLogEntry(AndroidLogcat.LogEntry entry)
        {
            Regex re = new Regex(@"at.*\s([^\s]+):(\d+)");
            var match = re.Match(entry.message);
            if (match.Success)
                UnityEditorInternal.InternalEditorUtility.TryOpenErrorFileFromConsole(match.Groups[1].Value, Int32.Parse(match.Groups[2].Value));
        }
    }
}
#endif
