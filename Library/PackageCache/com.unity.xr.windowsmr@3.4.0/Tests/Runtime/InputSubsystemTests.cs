#if JENKINS
using UnityEngine;
using UnityEngine.XR;
using UnityEngine.XR.Management;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

namespace UnityEngine.XR.WindowsMR.Tests
{
    public class InputSubsystemTests : TestBaseSetup
    {
        [UnityTest]
        public IEnumerator TestInputSubsystem()
        {
            yield return new WaitForSeconds(1);
            Assert.IsNotNull(ActiveLoader);
            XRInputSubsystem inputSub =ActiveLoader.GetLoadedSubsystem<XRInputSubsystem>();
            Assert.IsNotNull(inputSub);
        }

        [UnityTest]
        public IEnumerator TestDeviceTrackingOriginMode()
        {
            yield return new WaitForSeconds(1);
            XRInputSubsystem inputSub = ActiveLoader.GetLoadedSubsystem<XRInputSubsystem>();
            if (inputSub.GetTrackingOriginMode() == TrackingOriginModeFlags.Device)
            {
                List<Vector3> boundaryPoints = new List<Vector3>();
                inputSub.TryGetBoundaryPoints(boundaryPoints);
                Assert.IsTrue(boundaryPoints.Count == 0);
            }
        }

        [UnityTest]
        public IEnumerator TestFloorTrackingOriginMode()
        {
            yield return new WaitForSeconds(1);
            XRInputSubsystem inputSub = ActiveLoader.GetLoadedSubsystem<XRInputSubsystem>();
            if (inputSub.GetTrackingOriginMode() == TrackingOriginModeFlags.Floor)
            {
                List<Vector3> boundaryPoints = new List<Vector3>();
                inputSub.TryGetBoundaryPoints(boundaryPoints);
                Assert.IsFalse(boundaryPoints.Count == 0);
            }
        }

        [UnityTest]
        public IEnumerator TestBoundaryIsAtGround()
        {
            yield return new WaitForSeconds(1);
            XRInputSubsystem inputSub = ActiveLoader.GetLoadedSubsystem<XRInputSubsystem>();
            if (inputSub.GetTrackingOriginMode() == TrackingOriginModeFlags.Floor)
            {
                List<Vector3> boundaryPoints = new List<Vector3>();
                inputSub.TryGetBoundaryPoints(boundaryPoints);
                for(int i = 0; i < boundaryPoints.Count; i++)
                {
                    Assert.That( boundaryPoints[i].y, Is.EqualTo(0.0f).Within(1).Ulps);
                }
            }
        }
    }

}
#endif //JENKINS
