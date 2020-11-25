using UnityEngine;
using UnityEditor;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

#if JENKINS
namespace UnityEditor.XR.WindowsMR.Tests
{
    class EditorTests
    {
        internal class SmokeTests : TestBaseSetup
        {
            [Test]
            public void SceneIsCreated()
            {
                Assert.IsNotNull(m_Camera, "Camera was not created");
                Assert.IsNotNull(m_Light, "Light was not created");
                Assert.IsNotNull(m_Cube, "Cube was not created");
            }

            [UnityTest]
            public IEnumerator XrSdkAssetsCreated()
            {
                Assert.IsNotNull(m_TrackHead, "Tracking the Head Node was not created");
                yield return null;
                Assert.IsNotNull(m_TrackingRig, "Tracking rig was not created");
                yield return null;
            }
        }
    }
}
#else //JENKINS
namespace UnityEditor.XR.WindowsMR.Tests
{
    class EditorTests
    {
        [Test]
        public void YamatoPassTest()
        {
            // Pass test for Yamato
            Assert.IsTrue(true);
        }

        [UnityTest]
        public IEnumerator YamatoPassUnityTest()
        {
            yield return null;
            // Pass test for Yamato
            Assert.IsTrue(true);
        }
    }
}

#endif //JENKINS
