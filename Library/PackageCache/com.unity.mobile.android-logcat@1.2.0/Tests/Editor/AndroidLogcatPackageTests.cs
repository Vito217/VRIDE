using NUnit.Framework;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using Unity.Android.Logcat;
using UnityEngine;


internal class AndroidLogcatPackageTests : AndroidLogcatRuntimeTestBase
{
    [Test]
    public void DeadPackagesAreCleanupCorrectly()
    {
        try
        {
            InitRuntime();

            const int kPackagesToCreate = 10;
            var fakeDevice = new AndroidLogcatFakeDevice90("androiddevice0");
            for (int i = 0; i < kPackagesToCreate; i++)
            {
                var d = m_Runtime.ProjectSettings.CreatePackageInformation("com.unity.test" + i, i + 1, fakeDevice);
            }

            // All packages are alive, calling cleanup dead packages, shouldn't clean anything
            m_Runtime.ProjectSettings.CleanupDeadPackagesForDevice(fakeDevice);
            var packages = m_Runtime.ProjectSettings.GetKnownPackages(fakeDevice);
            Assert.AreEqual(kPackagesToCreate, packages.Count);

            foreach (var p in packages)
                p.SetExited();

            m_Runtime.ProjectSettings.CleanupDeadPackagesForDevice(fakeDevice);

            Assert.AreEqual(AndroidLogcatProjectSettings.kMaxExitedPackages, packages.Count);

            // Check that recent packages are still there, only the old packages should be removed
            foreach (var p in packages)
                Assert.IsTrue(p.processId > 5);
        }
        finally
        {
            ShutdownRuntime();
        }
    }
}
