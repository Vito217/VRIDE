#if JENKINS
using UnityEngine;
using UnityEngine.XR;
using UnityEngine.TestTools;
using UnityEngine.XR.Management;
using UnityEngine.XR.WindowsMR;

using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

namespace UnityEngine.XR.WindowsMR.Tests
{
    internal class EnvironmentTests : TestBaseSetup
    {
        [UnityTest]
        public IEnumerator HolographicSpaceAccess()
        {
            yield return new WaitForEndOfFrame();
            Assert.IsTrue(WindowsMREnvironment.HolographicSpace.ToInt64() != 0);
        }

        [UnityTest]
        public IEnumerator OriginSpatialCoordinateSystemAccess()
        {
            yield return new WaitForEndOfFrame();
            Assert.IsTrue(WindowsMREnvironment.OriginSpatialCoordinateSystem.ToInt64() != 0);
        }

        [UnityTest]
        public IEnumerator RenderHolographicFrameAccess()
        {
            yield return new WaitForSeconds(1);
            Assert.IsTrue(WindowsMREnvironment.CurrentHolographicRenderFrame.ToInt64() != 0);
        }
    
        [UnityTest]
        public IEnumerator SimulationHolographicFrameAccess()
        {
            yield return new WaitForSeconds(1);
            Assert.IsTrue(WindowsMREnvironment.CurrentHolographicSimulationFrame.ToInt64() != 0);
        }
    }

}
#endif //JENKINS
