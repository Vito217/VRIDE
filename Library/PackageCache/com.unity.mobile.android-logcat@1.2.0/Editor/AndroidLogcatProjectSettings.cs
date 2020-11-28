#if PLATFORM_ANDROID
using System;
using System.IO;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEditor.Android;
using System.Collections.Generic;
using System.Linq;
using static Unity.Android.Logcat.AndroidLogcatConsoleWindow;

namespace Unity.Android.Logcat
{
    [Serializable]
    internal class AndroidLogcatProjectSettings
    {
        internal const int kMaxExitedPackages = 5;

        [SerializeField]
        private string m_SelectedDeviceId;
        [SerializeField]
        private PackageInformation m_SelectedPackage;
        [SerializeField]
        private AndroidLogcat.Priority m_SelectedPriority;
        private Dictionary<string, List<PackageInformation>> m_KnownPackages;
        [SerializeField]
        private List<PackageInformation> m_KnownPackagesForSerialization;
        [SerializeField]
        private AndroidLogcatTags m_Tags;
        [SerializeField]
        private AndroidLogcatMemoryViewerState m_MemoryViewerState;
        [SerializeField]
        private string m_Filter;
        [SerializeField]
        private bool m_FilterIsRegularExpression;
        [SerializeField]
        private List<ReordableListItem> m_SymbolPaths;

        public string LastSelectedDeviceId
        {
            set
            {
                m_SelectedDeviceId = value;
            }
            get
            {
                return m_SelectedDeviceId;
            }
        }

        public bool LastSelectedDeviceIdValid
        {
            get
            {
                return !string.IsNullOrEmpty(m_SelectedDeviceId);
            }
        }

        public PackageInformation LastSelectedPackage
        {
            set
            {
                m_SelectedPackage = value;
            }
            get
            {
                return m_SelectedPackage;
            }
        }

        public bool SelectedPackageValid
        {
            get
            {
                return m_SelectedPackage != null &&
                    !string.IsNullOrEmpty(m_SelectedPackage.deviceId) &&
                    m_SelectedPackage.processId > 0;
            }
        }

        public AndroidLogcat.Priority SelectedPriority
        {
            set
            {
                m_SelectedPriority = value;
            }
            get
            {
                return m_SelectedPriority;
            }
        }

        private void RefreshPackagesForSerialization()
        {
            m_KnownPackagesForSerialization = new List<PackageInformation>();
            foreach (var p in m_KnownPackages)
            {
                m_KnownPackagesForSerialization.AddRange(p.Value);
            }
        }

        public IReadOnlyList<PackageInformation> GetKnownPackages(IAndroidLogcatDevice device)
        {
            return GetOrCreatePackagesForDevice(device);
        }

        private List<PackageInformation> GetOrCreatePackagesForDevice(IAndroidLogcatDevice device)
        {
            if (device == null)
                return new List<PackageInformation>();

            List<PackageInformation> packages = null;
            if (!m_KnownPackages.TryGetValue(device.Id, out packages))
            {
                packages = new List<PackageInformation>();
                m_KnownPackages[device.Id] = packages;
            }
            return packages;
        }

        public void CleanupDeadPackagesForDevice(IAndroidLogcatDevice device)
        {
            if (device == null)
                return;

            List<PackageInformation> packages = null;
            if (!m_KnownPackages.TryGetValue(device.Id, out packages))
                return;

            int deadPackageCount = 0;

            for (int i = 0; i < packages.Count; i++)
            {
                if (packages[i].IsAlive() == false)
                    deadPackageCount++;
            }

            // Need to remove the package which were added first, since they are the oldest packages
            int deadPackagesToRemove = deadPackageCount - kMaxExitedPackages;
            if (deadPackagesToRemove <= 0)
                return;

            for (int i = 0; i < packages.Count && deadPackagesToRemove > 0;)
            {
                if (packages[i].IsAlive())
                {
                    i++;
                    continue;
                }

                deadPackagesToRemove--;
                packages.RemoveAt(i);
            }

            RefreshPackagesForSerialization();
        }

        public PackageInformation CreatePackageInformation(string packageName, int pid, IAndroidLogcatDevice device)
        {
            if (pid <= 0)
                return null;

            var packages = GetOrCreatePackagesForDevice(device);
            PackageInformation info = packages.FirstOrDefault(package => package.processId == pid);
            if (info != null)
                return info;

            var newPackage = new PackageInformation()
            {
                name = packageName,
                processId = pid,
                deviceId = device.Id
            };

            packages.Add(newPackage);
            RefreshPackagesForSerialization();
            return newPackage;
        }

        private static Dictionary<string, List<PackageInformation>> PackagesToDictionary(List<PackageInformation> allPackages)
        {
            var dictionaryPackages = new Dictionary<string, List<PackageInformation>>();
            foreach (var p in allPackages)
            {
                List<PackageInformation> packages;
                if (!dictionaryPackages.TryGetValue(p.deviceId, out packages))
                {
                    packages = new List<PackageInformation>();
                    dictionaryPackages[p.deviceId] = packages;
                }
                packages.Add(p);
            }

            return dictionaryPackages;
        }

        public AndroidLogcatTags Tags
        {
            set
            {
                m_Tags = value;
            }
            get
            {
                return m_Tags;
            }
        }

        public AndroidLogcatMemoryViewerState MemoryViewerState
        {
            set
            {
                m_MemoryViewerState = value;
            }
            get
            {
                return m_MemoryViewerState;
            }
        }

        public string Filter
        {
            set
            {
                m_Filter = value;
            }
            get
            {
                return m_Filter;
            }
        }

        public bool FilterIsRegularExpression
        {
            set
            {
                m_FilterIsRegularExpression = value;
            }
            get
            {
                return m_FilterIsRegularExpression;
            }
        }

        public List<ReordableListItem> SymbolPaths
        {
            get => m_SymbolPaths;
        }

        internal AndroidLogcatProjectSettings()
        {
            Reset();
        }

        internal void Reset()
        {
            m_SelectedDeviceId = string.Empty;
            m_SelectedPriority = AndroidLogcat.Priority.Verbose;
            m_Tags = new AndroidLogcatTags();
            m_KnownPackages = new Dictionary<string, List<PackageInformation>>();
            m_MemoryViewerState = new AndroidLogcatMemoryViewerState();
            m_SymbolPaths = new List<ReordableListItem>();
        }

        internal static AndroidLogcatProjectSettings Load(string path)
        {
            if (!File.Exists(path))
                return null;

            var jsonString = File.ReadAllText(path);
            if (string.IsNullOrEmpty(jsonString))
                return null;

            try
            {
                var settings = new AndroidLogcatProjectSettings();
                JsonUtility.FromJsonOverwrite(jsonString, settings);
                settings.m_KnownPackages = PackagesToDictionary(settings.m_KnownPackagesForSerialization);
                return settings;
            }
            catch (Exception ex)
            {
                AndroidLogcatInternalLog.Log("Load Preferences from Json failed: " + ex.Message);
            }
            return null;
        }

        internal static void Save(AndroidLogcatProjectSettings settings, string path, AndroidLogcatRuntimeBase runtime)
        {
            if (settings == null)
                throw new NullReferenceException(nameof(settings));

            var jsonString = JsonUtility.ToJson(settings, true);
            if (string.IsNullOrEmpty(jsonString))
                return;

            File.WriteAllText(path, jsonString);
        }
    }
}

#endif
