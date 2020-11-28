#if UNITY_ANDROID
using System;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace Unity.Android.Logcat
{
    [Serializable]
    internal class ReordableListItem
    {
        [SerializeField]
        internal string Name;

        [SerializeField]
        internal bool Enabled;
    }

    internal class AndroidLogcatReordableList
    {
        private const float ButtonWidth = 25;
        private List<ReordableListItem> m_DataSource;
        private int m_SelectedIndex = -1;
        private string m_InputFieldText = String.Empty;
        private const string kInputTextFieldControlId = "ReordableListInputTextFieldControl";
        private static GUIContent kIconToolbarMinus = EditorGUIUtility.TrIconContent("Toolbar Minus");
        private static GUIContent kIconToolbarPlus = EditorGUIUtility.TrIconContent("Toolbar Plus");

        public Vector2 m_ScrollPosition = Vector2.zero;

        protected bool ShowEntryGUI { get; set; }

        protected bool ShowResetGUI { get; set; }

        public AndroidLogcatReordableList(List<ReordableListItem> dataSource)
        {
            ShowEntryGUI = true;
            m_DataSource = dataSource;
        }

        private GUILayoutOption[] ButtonStyles
        {
            get
            {
                return new GUILayoutOption[] { GUILayout.Width(ButtonWidth), GUILayout.Height(ButtonWidth) };
            }
        }

        public string CurrentItemName
        {
            get
            {
                if (m_SelectedIndex >= m_DataSource.Count)
                    m_SelectedIndex = -1;

                if (m_SelectedIndex == -1)
                    return m_InputFieldText;

                return m_DataSource[m_SelectedIndex].Name;
            }
            set
            {
                if (m_SelectedIndex == -1)
                    m_InputFieldText = value;
                else
                    m_DataSource[m_SelectedIndex].Name = value;
            }
        }

        void DoListGUI()
        {
            var currentEvent = Event.current;
            GUILayout.BeginVertical();
            m_ScrollPosition = GUILayout.BeginScrollView(m_ScrollPosition, false, false,
                GUIStyle.none,
                GUI.skin.verticalScrollbar,
                GUILayout.ExpandWidth(true));
            if (m_DataSource.Count == 0)
                DoListGUIWhenEmpty();

            for (int i = 0; i < m_DataSource.Count; i++)
            {
                var item = m_DataSource[i];
                EditorGUILayout.BeginHorizontal();
                GUILayout.Space(8);

                EditorGUILayout.BeginHorizontal();
                var toggleRect = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.tagToggleStyle);
                GUILayout.Space(4);
                var nameRect = GUILayoutUtility.GetRect(GUIContent.none, AndroidLogcatStyles.tagEntryStyle, GUILayout.ExpandWidth(true));
                EditorGUILayout.EndHorizontal();

                var entryRect = GUILayoutUtility.GetLastRect();
                EditorGUILayout.EndHorizontal();

                if (currentEvent.type == EventType.Repaint)
                {
                    if (m_SelectedIndex == i)
                        AndroidLogcatStyles.tagEntryBackground.Draw(entryRect, false, false, true, false);
                    else
                    {
                        if (i % 2 == 0)
                            AndroidLogcatStyles.tagEntryBackgroundEven.Draw(entryRect, false, false, false, false);
                        else
                            AndroidLogcatStyles.tagEntryBackgroundOdd.Draw(entryRect, false, false, false, false);
                    }
                }

                EditorGUI.BeginChangeCheck();
                item.Enabled = GUI.Toggle(toggleRect, item.Enabled, GUIContent.none, AndroidLogcatStyles.tagToggleStyle);
                if (EditorGUI.EndChangeCheck())
                    m_SelectedIndex = i;
                GUI.Label(nameRect, item.Name, AndroidLogcatStyles.tagEntryStyle);

                DoMouseEvent(entryRect, i);
            }
            //GUILayout.FlexibleSpace();
            GUILayout.EndScrollView();

            var msg = ValidateItem(CurrentItemName);
            if (!string.IsNullOrEmpty(msg))
                EditorGUILayout.HelpBox(msg, MessageType.Error, true);

            GUILayout.EndVertical();
        }

        protected virtual string ValidateItem(string item)
        {
            return string.Empty;
        }

        protected virtual void DoListGUIWhenEmpty()
        {
        }

        protected void AddItem(string name)
        {
            m_DataSource.Add(new ReordableListItem() { Name = name, Enabled = true });
            m_InputFieldText = string.Empty;
            m_SelectedIndex = m_DataSource.Count - 1;
            GUIUtility.keyboardControl = 0;
        }

        protected virtual void OnPlusButtonClicked()
        {
            if (!string.IsNullOrEmpty(CurrentItemName))
                AddItem(CurrentItemName);
        }

        protected virtual void OnResetButtonClicked()
        {
        }

        void DoButtonsGUI()
        {
            var currentEvent = Event.current;
            bool hitEnter = currentEvent.type == EventType.KeyDown && (currentEvent.keyCode == KeyCode.Return || currentEvent.keyCode == KeyCode.KeypadEnter);
            EditorGUILayout.BeginVertical(GUILayout.Width(ButtonWidth), GUILayout.Height(140));
            GUILayout.Space(2);
            if (GUILayout.Button(kIconToolbarPlus, ButtonStyles) || (hitEnter && GUI.GetNameOfFocusedControl() == kInputTextFieldControlId))
            {
                OnPlusButtonClicked();
            }

            EditorGUI.BeginDisabledGroup(m_SelectedIndex == -1);
            if (GUILayout.Button(kIconToolbarMinus, ButtonStyles))
            {
                RemoveSelected(m_SelectedIndex);
                GUIUtility.keyboardControl = 0;
            }
            EditorGUI.EndDisabledGroup();

            EditorGUI.BeginDisabledGroup(m_DataSource.Count <= 1 || m_SelectedIndex == -1);
            if (GUILayout.Button(AndroidLogcatStyles.kIconToolbarUp, ButtonStyles))
            {
                if (m_SelectedIndex > 0)
                {
                    var temp = m_DataSource[m_SelectedIndex];
                    m_DataSource[m_SelectedIndex] = m_DataSource[m_SelectedIndex - 1];
                    m_DataSource[m_SelectedIndex - 1] = temp;
                    m_SelectedIndex--;
                }
            }

            if (GUILayout.Button(AndroidLogcatStyles.kIconToolbarDown, ButtonStyles))
            {
                if (m_SelectedIndex < m_DataSource.Count - 1)
                {
                    var temp = m_DataSource[m_SelectedIndex];
                    m_DataSource[m_SelectedIndex] = m_DataSource[m_SelectedIndex + 1];
                    m_DataSource[m_SelectedIndex + 1] = temp;
                    m_SelectedIndex++;
                }
            }

            EditorGUI.EndDisabledGroup();

            if (ShowResetGUI)
            {
                if (GUILayout.Button(AndroidLogcatStyles.kIconReset, ButtonStyles))
                {
                    OnResetButtonClicked();
                }
            }

            EditorGUILayout.EndVertical();
        }

        protected void DoEntryGUI()
        {
            GUI.SetNextControlName(kInputTextFieldControlId);
            CurrentItemName = EditorGUILayout.TextField(CurrentItemName, GUILayout.Height(AndroidLogcatStyles.kTagEntryFixedHeight + 2));
        }

        public void OnGUI(float height)
        {
            EditorGUILayout.BeginVertical(GUILayout.Height(height));
            if (ShowEntryGUI)
                DoEntryGUI();

            EditorGUILayout.BeginHorizontal();
            DoListGUI();
            var listrc = GUILayoutUtility.GetLastRect();

            DoButtonsGUI();
            var buttonsrc = GUILayoutUtility.GetLastRect();

            // There's somekind of bug with layouting, ensure box height is not less then the height of button layout height
            var rc = new Rect(listrc.x, listrc.y, listrc.width, Math.Max(listrc.height, buttonsrc.height));
            GUI.Box(new Rect(rc.x + 4, rc.y, rc.width - 4, rc.height), GUIContent.none, EditorStyles.helpBox);
            EditorGUILayout.EndHorizontal();

            EditorGUILayout.EndVertical();
        }

        private void DoMouseEvent(Rect rect, int tagIndex)
        {
            var e = Event.current;
            if (e.type == EventType.MouseDown && rect.Contains(e.mousePosition))
            {
                switch (e.button)
                {
                    case 0:
                        m_SelectedIndex = (m_SelectedIndex == tagIndex) ? -1 : tagIndex;
                        e.Use();
                        GUIUtility.keyboardControl = 0;
                        break;
                }
            }
        }

        public bool RemoveSelected(int index)
        {
            if (index < 0 || index >= m_DataSource.Count)
                return false;

            m_DataSource.RemoveAt(index);
            if (m_DataSource.Count == 0)
                m_SelectedIndex = -1;
            else
                m_SelectedIndex = (int)Mathf.Clamp(m_SelectedIndex, 0, m_DataSource.Count - 1);

            return true;
        }
    }
}
#endif
