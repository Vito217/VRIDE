using System;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using Unity.Android.Logcat;

public class AndroidLogcatGeneralTests
{
    [Test]
    public void ParseVersionTests()
    {
        var values = new KeyValuePair<Version, string>[]
        {
            new KeyValuePair<Version, string>(new Version(1, 0), "1"),
            new KeyValuePair<Version, string>(new Version(1, 2), "1.2"),
            new KeyValuePair<Version, string>(new Version(1, 2, 3), "1.2.3"),
            new KeyValuePair<Version, string>(new Version(1, 2, 3, 4), "1.2.3.4")
        };

        foreach (var v in values)
        {
            Assert.AreEqual(v.Key, AndroidLogcatUtilities.ParseVersionLegacy(v.Value));
            Assert.AreEqual(v.Key, AndroidLogcatUtilities.ParseVersion(v.Value));
        }
    }
}
