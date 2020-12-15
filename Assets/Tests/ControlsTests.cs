using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.SceneManagement;
using System.Collections.Generic;
using UnityEngine.XR;
using UnityEngine.XR.Management;

namespace Tests
{
    public class ControlsTests
    {
        public GameObject user;

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
