#if JENKINS
using UnityEngine;
using UnityEngine.XR;
using UnityEngine.XR.Management;
using UnityEngine.TestTools;

using UnityEngine.XR.WSA;

using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

namespace UnityEngine.XR.WindowsMR.Tests
{
    internal class DisplaySubsystemTests : TestBaseSetup
    {
        public IEnumerator ContentProtection()
        {
            var disp = ActiveLoader.GetLoadedSubsystem<XRDisplaySubsystem>();
            Assert.IsTrue(disp != null);
            yield return new WaitForEndOfFrame();

            bool oldContentState = disp.contentProtectionEnabled;
            disp.contentProtectionEnabled = !oldContentState;

            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();

            bool newContentState = disp.contentProtectionEnabled;
            Assert.IsTrue(oldContentState != newContentState);
            disp.contentProtectionEnabled = oldContentState;

            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();

            newContentState = disp.contentProtectionEnabled;
            Assert.IsTrue(oldContentState == newContentState);
        }

        [UnityTest]
        public IEnumerator ReprojectionMode()
        {
            var disp = ActiveLoader.GetLoadedSubsystem<XRDisplaySubsystem>();
            Assert.IsTrue(disp != null);

            XRDisplaySubsystem.ReprojectionMode oldContentState = disp.reprojectionMode;
            XRDisplaySubsystem.ReprojectionMode newContentState = oldContentState;

            switch (oldContentState)
            {
                case XRDisplaySubsystem.ReprojectionMode.Unspecified:
                    newContentState = XRDisplaySubsystem.ReprojectionMode .OrientationOnly;
                    break;

                case XRDisplaySubsystem.ReprojectionMode .OrientationOnly:
                    newContentState = XRDisplaySubsystem.ReprojectionMode .PositionAndOrientation;
                    break;

                case XRDisplaySubsystem.ReprojectionMode .PositionAndOrientation:
                    newContentState = XRDisplaySubsystem.ReprojectionMode .None;
                    break;

                case XRDisplaySubsystem.ReprojectionMode.None:
                    newContentState = XRDisplaySubsystem.ReprojectionMode.OrientationOnly;
                    break;
            }

            disp.reprojectionMode = newContentState;

            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();

            var currentContentState = disp.reprojectionMode;
            Assert.IsTrue(currentContentState == oldContentState || currentContentState == newContentState);
            newContentState = oldContentState;
            disp.reprojectionMode = newContentState;

            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();
            yield return new WaitForEndOfFrame();

            newContentState = disp.reprojectionMode;
            Assert.IsTrue(oldContentState == newContentState);
        }


        [UnityTest]
        public IEnumerator FocusPoint()
        {
            var disp = ActiveLoader.GetLoadedSubsystem<XRDisplaySubsystem>();
            Assert.IsTrue(disp != null);
            yield return new WaitForEndOfFrame();

            disp.SetFocusPlane(Vector3.forward, Vector3.up, Vector3.right);
            yield return new WaitForEndOfFrame();
        }

        [UnityTest]
        public IEnumerator DisplayOpaque()
        {
            var disp = ActiveLoader.GetLoadedSubsystem<XRDisplaySubsystem>();
            Assert.IsTrue(disp != null);
            yield return new WaitForEndOfFrame();

            // Will fail if not on a HMD but shouldn't be run off machine anyway.
            Assert.IsTrue(disp.displayOpaque);
        }

    }

}
#endif //JENKINS
