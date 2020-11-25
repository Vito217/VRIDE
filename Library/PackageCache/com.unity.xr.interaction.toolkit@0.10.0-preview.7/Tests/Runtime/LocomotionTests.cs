using NUnit.Framework;
using UnityEngine.TestTools;
using System.Collections;
using UnityEngine.TestTools.Utils;

namespace UnityEngine.XR.Interaction.Toolkit.Tests
{
    [TestFixture]
    public class LocomotionTests
    {
        [TearDown]
        public void TearDown()
        {
            TestUtilities.DestroyAllInteractionObjects();
        }

        [UnityTest]
        public IEnumerator TeleportToAnchorWithStraightLineAndMatchTargetUp()
        {
            var manager = TestUtilities.CreateInteractionManager();
            var xrRig = TestUtilities.CreateXRRig();

            // config teleportation on XR rig
            LocomotionSystem locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            TeleportationProvider teleProvider = xrRig.gameObject.AddComponent<TeleportationProvider>();
            teleProvider.system = locoSys;
                        
            // interactor
            var interactor = TestUtilities.CreateRayInteractor();

            interactor.transform.SetParent(xrRig.cameraFloorOffsetObject.transform);
            interactor.lineType = XRRayInteractor.LineType.StraightLine;            

            // controller
            var controller = interactor.GetComponent<XRController>();

            // create teleportation anchors
            var teleAnchor = TestUtilities.CreateTeleportAnchorPlane();
            teleAnchor.interactionManager = manager;
            teleAnchor.teleportationProvider = teleProvider;
            teleAnchor.matchOrientation = MatchOrientation.TargetUp;

            // set teleportation anchor plane in the forward direction of controller
            teleAnchor.transform.position = interactor.transform.forward + Vector3.down;
            teleAnchor.transform.Rotate(-45, 0, 0, Space.World);

            var controllerRecorder = TestUtilities.CreateControllerRecorder(controller, (recording) =>
            {
                recording.AddRecordingFrame(0.0f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.1f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(float.MaxValue, Vector3.zero, Quaternion.identity,
                    false, false, false);
            });
            controllerRecorder.isPlaying = true;

            // wait for 1s to make sure the recorder simulates the action
            yield return new WaitForSeconds(1f);
            Vector3 cameraPosAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;
            Assert.That(xrRig.cameraGameObject.transform.position == teleAnchor.transform.position + cameraPosAdjustment);
            Assert.That(xrRig.rig.transform.up == teleAnchor.transform.up);
            Vector3 projectedCameraForward = Vector3.ProjectOnPlane(xrRig.cameraGameObject.transform.forward, teleAnchor.transform.up);
            Assert.That(projectedCameraForward.normalized == teleAnchor.transform.forward);

        }

        [UnityTest]
        public IEnumerator TeleportToAnchorWithStraightLineAndMatchWorldSpace()
        {
            var manager = TestUtilities.CreateInteractionManager();
            var xrRig = TestUtilities.CreateXRRig();

            // config teleportation on XR rig
            LocomotionSystem locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            TeleportationProvider teleProvider = xrRig.gameObject.AddComponent<TeleportationProvider>();
            teleProvider.system = locoSys;
                        
            // interactor
            var interactor = TestUtilities.CreateRayInteractor();

            interactor.transform.SetParent(xrRig.cameraFloorOffsetObject.transform);
            interactor.lineType = XRRayInteractor.LineType.StraightLine;            

            // controller
            var controller = interactor.GetComponent<XRController>();

            // create teleportation anchors
            var teleAnchor = TestUtilities.CreateTeleportAnchorPlane();
            teleAnchor.interactionManager = manager;
            teleAnchor.teleportationProvider = teleProvider;
            teleAnchor.matchOrientation = MatchOrientation.WorldSpaceUp;

            // set teleportation anchor plane in the forward direction of controller
            teleAnchor.transform.position = interactor.transform.forward + Vector3.down;
            teleAnchor.transform.Rotate(-45, 0, 0, Space.World);

            var controllerRecorder = TestUtilities.CreateControllerRecorder(controller, (recording) =>
            {
                recording.AddRecordingFrame(0.0f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.1f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(float.MaxValue, Vector3.zero, Quaternion.identity,
                    false, false, false);
            });
            controllerRecorder.isPlaying = true;

            // wait for 1s to make sure the recorder simulates the action
            yield return new WaitForSeconds(1f);
            Vector3 cameraPosAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;
            Assert.AreEqual(teleAnchor.transform.position + cameraPosAdjustment, xrRig.cameraGameObject.transform.position, "XR Rig position");
            Assert.AreEqual(Vector3.up, xrRig.rig.transform.up, "XR Rig up vector");
            Vector3 projectedCameraForward = Vector3.ProjectOnPlane(xrRig.cameraGameObject.transform.forward, teleAnchor.transform.up);
            Assert.AreEqual(Vector3.forward, xrRig.cameraGameObject.transform.forward, "Projected forward");

        }

        [UnityTest]
        public IEnumerator TeleportToAnchorWithStraightLineAndMatchTargetUpAndForward()
        {
            var manager = TestUtilities.CreateInteractionManager();
            var xrRig = TestUtilities.CreateXRRig();

            // config teleportation on XR rig
            LocomotionSystem locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            TeleportationProvider teleProvider = xrRig.gameObject.AddComponent<TeleportationProvider>();
            teleProvider.system = locoSys;
                        
            // interactor
            var interactor = TestUtilities.CreateRayInteractor();

            interactor.transform.SetParent(xrRig.cameraFloorOffsetObject.transform);
            interactor.lineType = XRRayInteractor.LineType.StraightLine;            

            // controller
            var controller = interactor.GetComponent<XRController>();

            // create teleportation anchors
            var teleAnchor = TestUtilities.CreateTeleportAnchorPlane();
            teleAnchor.interactionManager = manager;
            teleAnchor.teleportationProvider = teleProvider;
            teleAnchor.matchOrientation = MatchOrientation.TargetUpAndForward;

            // set teleportation anchor plane in the forward direction of controller
            teleAnchor.transform.position = interactor.transform.forward + Vector3.down;
            teleAnchor.transform.Rotate(-45, 0, 0, Space.World);

            var controllerRecorder = TestUtilities.CreateControllerRecorder(controller, (recording) =>
            {
                recording.AddRecordingFrame(0.0f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.1f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(float.MaxValue, Vector3.zero, Quaternion.identity,
                    false, false, false);
            });
            controllerRecorder.isPlaying = true;

            // wait for 1s to make sure the recorder simulates the action
            yield return new WaitForSeconds(1f);
            Vector3 cameraPosAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;
            Assert.That(xrRig.cameraGameObject.transform.position == teleAnchor.transform.position + cameraPosAdjustment);
            Assert.That(xrRig.rig.transform.up == teleAnchor.transform.up);
            Vector3 projectedCameraForward = Vector3.ProjectOnPlane(xrRig.cameraGameObject.transform.forward, teleAnchor.transform.up);
            Assert.That(projectedCameraForward.normalized == teleAnchor.transform.forward);

        }

        [UnityTest]
        public IEnumerator TeleportToAnchorWithProjectile()
        {
            var manager = TestUtilities.CreateInteractionManager();
            var xrRig = TestUtilities.CreateXRRig();

            // config teleportation on XR rig
            LocomotionSystem locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            TeleportationProvider teleProvider = xrRig.gameObject.AddComponent<TeleportationProvider>();
            teleProvider.system = locoSys;

            // interactor
            var interactor = TestUtilities.CreateRayInteractor();

            interactor.transform.SetParent(xrRig.cameraFloorOffsetObject.transform);
            interactor.lineType = XRRayInteractor.LineType.ProjectileCurve; // projectile curve            

            // controller
            var controller = interactor.GetComponent<XRController>();

            // create teleportation anchors
            var teleAnchor = TestUtilities.CreateTeleportAnchorPlane();
            teleAnchor.interactionManager = manager;
            teleAnchor.teleportationProvider = teleProvider;
            teleAnchor.matchOrientation = MatchOrientation.TargetUp;

            // set teleportation anchor plane
            teleAnchor.transform.position = interactor.transform.forward + Vector3.down;
            teleAnchor.transform.Rotate(-90, 0, 0, Space.World);

            var controllerRecorder = TestUtilities.CreateControllerRecorder(controller, (recording) =>
            {
                recording.AddRecordingFrame(0.0f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.1f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(float.MaxValue, Vector3.zero, Quaternion.identity,
                    false, false, false);
            });
            controllerRecorder.isPlaying = true;

            // wait for 1s to make sure the recorder simulates the action
            yield return new WaitForSeconds(1f);

            Vector3 cameraPosAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;
            Assert.That(xrRig.cameraGameObject.transform.position == teleAnchor.transform.position + cameraPosAdjustment);
            Assert.That(xrRig.rig.transform.up == teleAnchor.transform.up);
        }

        [UnityTest]
        public IEnumerator TeleportToAnchorWithBezierCurve()
        {
            var manager = TestUtilities.CreateInteractionManager();
            var xrRig = TestUtilities.CreateXRRig();

            // config teleportation on XR rig
            LocomotionSystem locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            TeleportationProvider teleProvider = xrRig.gameObject.AddComponent<TeleportationProvider>();
            teleProvider.system = locoSys;

            // interactor
            var interactor = TestUtilities.CreateRayInteractor();

            interactor.transform.SetParent(xrRig.cameraFloorOffsetObject.transform);
            interactor.lineType = XRRayInteractor.LineType.BezierCurve; // projectile curve

            // controller
            var controller = interactor.GetComponent<XRController>();
            
            // create teleportation anchors
            var teleAnchor = TestUtilities.CreateTeleportAnchorPlane();
            teleAnchor.interactionManager = manager;
            teleAnchor.teleportationProvider = teleProvider;
            teleAnchor.matchOrientation = MatchOrientation.TargetUp;

            // set teleportation anchor plane
            teleAnchor.transform.position = interactor.transform.forward + Vector3.down;
            teleAnchor.transform.Rotate(-90, 0, 0, Space.World);

            var controllerRecorder = TestUtilities.CreateControllerRecorder(controller, (recording) =>
            {
                recording.AddRecordingFrame(0.0f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.1f, Vector3.zero, Quaternion.identity,
                    true, false, false);
                recording.AddRecordingFrame(0.2f, Vector3.zero, Quaternion.identity,
                    false, false, false);
                recording.AddRecordingFrame(float.MaxValue, Vector3.zero, Quaternion.identity,
                    false, false, false);
            });
            controllerRecorder.isPlaying = true;

            // wait for 1s to make sure the recorder simulates the action
            yield return new WaitForSeconds(1f);

            Vector3 cameraPosAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;
            Assert.That(xrRig.cameraGameObject.transform.position == teleAnchor.transform.position + cameraPosAdjustment);
            Assert.That(xrRig.rig.transform.up == teleAnchor.transform.up);
        }

        [UnityTest]
        public IEnumerator SnapTurn()
        {
            var xrRig = TestUtilities.CreateXRRig();

            // Config snap turn on XR rig
            var locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            locoSys.xrRig = xrRig;
            var snapProvider = xrRig.gameObject.AddComponent<DeviceBasedSnapTurnProvider>();
            snapProvider.system = locoSys;
            float turnAmount = snapProvider.turnAmount;

            snapProvider.FakeStartTurn(false);

            yield return new WaitForSeconds(0.1f);

            Assert.That(xrRig.transform.rotation.eulerAngles, Is.EqualTo(new Vector3(0f, turnAmount, 0f)).Using(Vector3ComparerWithEqualsOperator.Instance));
        }

        [UnityTest]
        public IEnumerator SnapTurnAround()
        {
            var xrRig = TestUtilities.CreateXRRig();

            // Config snap turn on XR rig
            var locoSys = xrRig.gameObject.AddComponent<LocomotionSystem>();
            locoSys.xrRig = xrRig;
            var snapProvider = xrRig.gameObject.AddComponent<DeviceBasedSnapTurnProvider>();
            snapProvider.system = locoSys;

            snapProvider.FakeStartTurnAround();

            yield return new WaitForSeconds(0.1f);

            Assert.That(xrRig.transform.rotation.eulerAngles, Is.EqualTo(new Vector3(0f, 180f, 0f)).Using(Vector3ComparerWithEqualsOperator.Instance));
        }
    }
}


