#if PLATFORM_ANDROID

using System;
using UnityEngine;

namespace Unity.Android.Logcat
{
    [Serializable]
    class TagInformation
    {
        [SerializeField]
        private string m_Name;

        [SerializeField]
        private bool m_Selected;

        public string Name
        {
            set
            {
                m_Name = value;
            }

            get
            {
                return m_Name;
            }
        }

        public bool Selected
        {
            set
            {
                m_Selected = value;
            }

            get
            {
                return m_Selected;
            }
        }

        public override string ToString()
        {
            var n = Name == null ? "<null>" : Name;
            return $"{n} {Selected}";
        }
    }
}
#endif
