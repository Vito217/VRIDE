using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.SceneManagement;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using TestProperties;
using UnityEngine.XR;
using UnityEngine.XR.Management;

namespace Tests
{
    public class ControlsTests
    {
        public GameObject user;

        // A Test behaves as an ordinary method
        [Test]
        public void ControlsTestsSimplePasses()
        {
            // Use the Assert class to test conditions
        }

        // A UnityTest behaves like a coroutine in Play Mode. In Edit Mode you can use
        // `yield return null;` to skip a frame.
        [UnityTest]
        public IEnumerator ControlsTestsWithEnumeratorPasses()
        {
            SceneManager.LoadScene("InitialScene", LoadSceneMode.Single);
            yield return null;
            Assert.IsTrue(SceneManager.GetActiveScene().name == "InitialScene");

            user = GameObject.Find("VRPlayer");
            yield return null;
            Assert.IsNotNull(user);

            List<XRDisplaySubsystem> displaySubsystems = new List<XRDisplaySubsystem>();
            SubsystemManager.GetInstances<XRDisplaySubsystem>(displaySubsystems);
            Assert.IsTrue(displaySubsystems.Count > 0);

            yield return null;
            Assert.IsTrue(XRGeneralSettings.Instance.Manager.activeLoader != null);
            yield return null;
            Assert.IsTrue(XRGeneralSettings.Instance.Manager.isInitializationComplete);
        }
    }
}
