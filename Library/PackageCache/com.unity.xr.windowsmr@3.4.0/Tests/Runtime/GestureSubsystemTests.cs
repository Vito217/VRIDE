#if JENKINS
using UnityEngine;
using UnityEngine.XR.InteractionSubsystems;
using UnityEngine.XR;
using UnityEngine.XR.Management;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

namespace Unity.XR.WindowsMR.Tests
{
    public class GestureSubsystemTests : TestBaseSetup
    {
        [Test]
        public void TestGestureSubsystem()
        {
            Assert.IsNotNull(ActiveLoader);
            XRGestureSubsystem gestureSub = ActiveLoader.GetLoadedSubsystem<XRGestureSubsystem>();
            Assert.IsNotNull(gestureSub);
        }
    }

}
#endif //JENKINS
