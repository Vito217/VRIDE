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
    [Serializable]
    public class ColumnData
    {
        [NonSerialized]
        public GUIContent content;

        public float width;

        [NonSerialized]
        // Updated automatically when we're moving the splitter
        public Rect itemSize = Rect.zero;

        [NonSerialized]
        public bool splitterDragging;

        [NonSerialized]
        public float splitterDragStartMouseValue;

        [NonSerialized]
        public float splitterDragStartWidthValue;

        public bool enabled = true;
    }
}
#endif
