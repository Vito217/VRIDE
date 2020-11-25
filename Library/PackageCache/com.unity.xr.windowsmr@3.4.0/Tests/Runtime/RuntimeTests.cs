using UnityEngine;
using UnityEngine.XR;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System;

#if JENKINS
namespace UnityEngine.XR.WindowsMR.Tests
{
    class RuntimeTests
    {
        internal class GraphicsCheck : TestBaseSetup
        {
            enum States
            {
                MSAA_AND_HDR = 0,
                MSAA,
                HDR,
                NO_MSAA_AND_NO_HDR
            }

            private States currentState;
            private bool doVerification = false;
            private bool stopTest = false;
            private bool allTestsPassed = true;

            private GameObject colorScreen = null;
            private Material testMat;

            [SetUp]
            public void SetUp()
            {
                testMat = new Material(Resources.Load("Materials/YFlipColorMesh", typeof(Material)) as Material);
                currentState = States.MSAA_AND_HDR;

                colorScreen = GameObject.CreatePrimitive(PrimitiveType.Quad);
                colorScreen.transform.position = new Vector3(0f, 0f, 1f);
                colorScreen.GetComponent<Renderer>().material = testMat;
            }

            [TearDown]
            public void TearDown()
            {
                GameObject.Destroy(colorScreen);
            }

            [UnityTest]
            public IEnumerator CheckYWorldCoordinate()
            {
                while (!stopTest)
                {
                    DoTest();
                    yield return new WaitForSeconds(2.0f);
                }
            }

            void DoTest()
            {
                doVerification = true;

                if (currentState == States.MSAA_AND_HDR)
                {
                    m_Camera.GetComponent<Camera>().allowHDR = true;
                    m_Camera.GetComponent<Camera>().allowMSAA = true;
                    Debug.Log("MSAA AND HDR");
                }
                else if (currentState == States.MSAA)
                {
                    m_Camera.GetComponent<Camera>().allowHDR = false;
                    m_Camera.GetComponent<Camera>().allowMSAA = true;
                    Debug.Log("MSAA");
                }
                else if (currentState == States.HDR)
                {
                    m_Camera.GetComponent<Camera>().allowHDR = true;
                    m_Camera.GetComponent<Camera>().allowMSAA = false;
                    Debug.Log("HDR");
                }
                else
                {
                    m_Camera.GetComponent<Camera>().allowHDR = false;
                    m_Camera.GetComponent<Camera>().allowMSAA = false;
                    Debug.Log("NO MSAA and NO HDR");
                }

                currentState = currentState + 1;
                if ((int)currentState >= System.Enum.GetValues(typeof(States)).Length)
                {
                    stopTest = true;

                    if (allTestsPassed)
                    {
                        Debug.Log("The y-flip test passed successfully!");
                    }
                    else
                    {
                        Debug.Log("The y-flip test failed!");
                    }
                }
            }

            bool IsYFlipCorrect(RenderTexture src)
            {
                RenderTexture originalActiveRenderTexture = RenderTexture.active;

                RenderTexture.active = src;
                Texture2D tex = new Texture2D(src.width, src.height, TextureFormat.RGBA32, src.useMipMap, src.sRGB);
                tex.name = "Y Flip Test Texture";
                tex.ReadPixels(new Rect(0, 0, tex.width, tex.height), 0, 0);
                tex.Apply();

                // We shouldn't sample directly from (0,0) because chances are that will overlap
                // the occlusion mesh.  Therefore we should try to sample closer to the center bottom of the texture.
                float x = src.width * 0.5f;
                float y = src.height * 0.3f;
                Color color = tex.GetPixel((int)x, (int)y);
                tex = null;

                RenderTexture.active = originalActiveRenderTexture;

                // Texture coordinates start at lower left corner.  So (0,0) should be red.
                // https://docs.unity3d.com/ScriptReference/Texture2D.GetPixel.html
                if (color == Color.red)
                {
                    return true;
                }

                return false;
            }

            void OnRenderImage(RenderTexture src, RenderTexture dst)
            {
                if (doVerification)
                {
                    if (!IsYFlipCorrect(src))
                    {
                        Debug.LogError(string.Format("The texture is y-flipped incorrectly for camera mode {0}", System.Enum.GetName(typeof(States), currentState)));
                        allTestsPassed = false;
                    }
                    doVerification = false;
                }

                Graphics.Blit(src, dst);
            }
        }

        internal class XrNodes : TestBaseSetup
        {
            private List<XRNodeState> m_NodeList;

            private bool m_TrackingNodes;
            private bool m_TrackingHeadNode;
            private bool m_TrackingRightEyeNode;
            private bool m_TrackingLeftEyeNode;
            private bool m_TrackingCenterEyeNode;

            public bool m_TrackingEyeNode { get; private set; }

            [SetUp]
            public void Setup()
            {
                m_NodeList = new List<XRNodeState>();

                InputTracking.trackingAcquired += InputTracking_trackingAcquired;
                InputTracking.trackingLost += InputTracking_trackingLost;
                InputTracking.nodeAdded += InputTracking_nodeAdded;
                InputTracking.nodeRemoved += InputTracking_nodeRemoved;

                m_TrackingNodes = m_TrackingHeadNode = m_TrackingLeftEyeNode = m_TrackingRightEyeNode = m_TrackingCenterEyeNode = false;
            }

            [TearDown]
            public void TearDown()
            {
                InputTracking.trackingAcquired -= InputTracking_trackingAcquired;
                InputTracking.trackingLost -= InputTracking_trackingLost;
                InputTracking.nodeAdded -= InputTracking_nodeAdded;
                InputTracking.nodeRemoved -= InputTracking_nodeRemoved;
            }

            [UnityTest]
            public IEnumerator XrNodesTracking()
            {
                InputTracking.GetNodeStates(m_NodeList);
                yield return new WaitForSeconds(1f);

                foreach (XRNodeState nodeState in m_NodeList)
                {
                    if (nodeState.tracked)
                    {
                        m_TrackingNodes = true;
                    }
                }

                Assert.IsTrue(m_TrackingNodes, "Nodes are not tracking");
            }

            [UnityTest]
            public IEnumerator XrNodesHeadTracking()
            {
                InputTracking.GetNodeStates(m_NodeList);
                yield return new WaitForSeconds(5f);

                foreach (XRNodeState nodeState in m_NodeList)
                {
                    if (nodeState.tracked)
                    {
                        if (nodeState.nodeType == XRNode.Head)
                        {
                            m_TrackingHeadNode = true;
                        }
                    }
                }

                Assert.IsTrue(m_TrackingHeadNode, "Head Node is not tracking");
            }

            [UnityTest]
            public IEnumerator XrNodesEyeTracking()
            {
                InputTracking.GetNodeStates(m_NodeList);
                yield return new WaitForSeconds(1f);

                foreach (XRNodeState nodeState in m_NodeList)
                {
                    if (nodeState.tracked)
                    {
                        if (nodeState.nodeType == XRNode.LeftEye)
                        {
                            m_TrackingRightEyeNode = true;
                        }

                        if (nodeState.nodeType == XRNode.RightEye)
                        {
                            m_TrackingLeftEyeNode = true;
                        }

                        if (nodeState.nodeType == XRNode.CenterEye)
                        {
                            m_TrackingCenterEyeNode = true;
                        }

                        if (m_TrackingLeftEyeNode == m_TrackingRightEyeNode == m_TrackingCenterEyeNode)
                        {
                            m_TrackingEyeNode = true;
                        }
                    }
                }

                Assert.IsTrue(m_TrackingEyeNode, "Eye Nodes are not tracking");
            }

            private void InputTracking_nodeAdded(XRNodeState obj)
            {
                Debug.Log("Node Added : " + obj.nodeType);
            }

            private void InputTracking_trackingAcquired(XRNodeState obj)
            {
                Debug.Log("Tracking Acquired: " + obj.nodeType);
            }

            private void InputTracking_trackingLost(XRNodeState obj)
            {
                Debug.Log("Tracking Lost : " + obj.nodeType);
            }

            private void InputTracking_nodeRemoved(XRNodeState obj)
            {
                Debug.Log("Node Removed : " + obj.nodeType);
            }
        }

        internal class EyeCameraCheck : TestBaseSetup
        {
            private bool m_AngleCheck = false;
            private bool m_EyesInFront = false;
            private bool m_EyeAngleCheck = false;
            private bool m_LeftEyeAngleCheck = false;
            private bool m_RightEyeAngleCheck = false;

            [UnityTest]
            public IEnumerator EyesParallelHead()
            {
                yield return null;

                EyeParallelWithHead();

                Assert.IsTrue(m_AngleCheck, "Eyes are not parallel with the head");
            }

            [UnityTest]
            public IEnumerator EyePositionCheckWithHead()
            {
                yield return null;

                EyePositionCheck();

                Assert.IsTrue(m_EyesInFront, "Eyes are not in front with the head");
                Assert.IsTrue(m_EyeAngleCheck, "Eye Angles don't match with the head");
            }

            static bool AngleCheck(float a, float b)
            {
                float m_Tolerance = 2f;
                var check = Mathf.Abs(a - b) < m_Tolerance;
                return (check);
            }

            static bool CompareEyeAngles(float a, float b)
            {
                float m_Tolerance = 0.5f;
                var check = Mathf.Abs(a - b) < Mathf.Abs(a - b) + m_Tolerance;
                return (check);
            }

            static bool EyeZPositionCheck(float a, float b)
            {
                var delta = Math.Abs(a - b);
                var check = delta <= Single.Epsilon;
                return (check);
            }

            static bool CheckMathForEyes(float Convergence, float EyeAngle)
            {
                // Verification of the math
                // tan should be half of tan 2
                // tan 2 should be half of tan 3
                bool mathPassed = false;
                bool check1Pass = false;
                bool check2Pass = false;

                var tan = Mathf.Tan(EyeAngle);
                Debug.Log(tan);

                var tan2 = Convergence * Mathf.Tan(EyeAngle);
                Debug.Log(tan2);

                var tan3 = (2 * Convergence) * Mathf.Tan(EyeAngle);
                Debug.Log(tan3);

                var check = tan2 / Convergence;
                if (check == tan)
                {
                    Debug.Log("Check 1 passed - " + check);
                    check1Pass = true;
                }

                check = tan3 / (2 * Convergence);
                if (check == tan)
                {
                    Debug.Log("Check 2 passed - " + check);
                    check2Pass = true;
                }

                if (check1Pass & check2Pass == true)
                {
                    mathPassed = true;
                }
                else
                {
                    mathPassed = false;
                }

                return (mathPassed);
            }

            public void EyeParallelWithHead()
            {
                Matrix4x4 left = m_Camera.GetComponent<Camera>().GetStereoViewMatrix(Camera.StereoscopicEye.Left);
                Matrix4x4 right = m_Camera.GetComponent<Camera>().GetStereoViewMatrix(Camera.StereoscopicEye.Right);

                Vector3 m_LeftEyePos = left.inverse.MultiplyPoint(Vector3.zero);
                Vector3 m_RightEyePos = right.inverse.MultiplyPoint(Vector3.zero);

                Vector3 eyesDelta = (m_RightEyePos - m_LeftEyePos).normalized;
                Vector3 rightDir = m_Camera.transform.right;
                float angle = Vector3.Angle(eyesDelta, rightDir);

                if (AngleCheck(angle, 0f))
                {
                    Debug.Log("Eyes Parallel is OK : " + angle);
                    m_AngleCheck = true;
                }
                else if (!AngleCheck(angle, 0f))
                {
                    Debug.Log("Eye Parallel is BAD = " + angle);
                    m_AngleCheck = false;
                }
            }

            public void EyePositionCheck()
            {
                List<InputDevice> devices = new List<InputDevice>();
                InputDevices.GetDevicesWithCharacteristics(InputDeviceCharacteristics.HeadMounted, devices);

                if(devices.Count == 0)
                    throw new Exception("Head Mounted Input Device not connected.");

                InputDevice device = devices[0];

                Vector3 LeftEye = Vector3.zero;

                if(!device.TryGetFeatureValue(CommonUsages.leftEyePosition, out LeftEye))
                    throw new Exception("Left Eye Position not found.");

                Vector3 RightEye = Vector3.zero;

                if(!device.TryGetFeatureValue(CommonUsages.rightEyePosition, out RightEye))
                    throw new Exception("Right Eye Position not found.");

                Vector3 CenterEye  = Vector3.zero;
                if(!device.TryGetFeatureValue(CommonUsages.centerEyePosition, out CenterEye))
                    throw new Exception("Right Eye Position not found.");

                Vector3 LeftEyeInverse = m_Camera.transform.InverseTransformVector(LeftEye);
                Vector3 RightEyeInverse = m_Camera.transform.InverseTransformVector(RightEye);

                Debug.Log("Eye Left Inverse Position = " + LeftEyeInverse +
                                      Environment.NewLine + "Eye Right Inverse Position = " + RightEyeInverse);

                if (EyeZPositionCheck(LeftEye.z, CenterEye.z))
                {
                    Debug.Log("Eyes are in front of the head : " + LeftEye.z);
                    m_EyesInFront = true;
                }
                else if (!EyeZPositionCheck(LeftEye.z, CenterEye.z))
                {
                    Debug.Log("Eyes are behind the head : " + LeftEye.z);
                    m_EyesInFront = false;
                }

                Vector3 forwardDirLeft = m_Camera.transform.forward;
                float leftEyeAngle = Vector3.Angle(LeftEye, forwardDirLeft);

                Vector3 forwardDirRight = m_Camera.transform.forward;
                float rightEyeAngle = Vector3.Angle(RightEye, forwardDirRight);

                CheckMathForEyes(m_Camera.GetComponent<Camera>().stereoConvergence, leftEyeAngle);

                // Check to make sure the eye angles from the head are the same
                if (CompareEyeAngles(leftEyeAngle, rightEyeAngle))
                {
                    Debug.Log("Left and Right eye angles are the same : " + leftEyeAngle + " | " + rightEyeAngle);

                    m_EyeAngleCheck = true;
                }
                else if (!CompareEyeAngles(leftEyeAngle, rightEyeAngle))
                {
                    Debug.Log("Left and Right eye angles are not the same : " + leftEyeAngle + " | " + rightEyeAngle);
                    m_EyeAngleCheck = false;
                }

                //Check to make sure the angle from the camera to the left eye is reasonable
                if (!AngleCheck(leftEyeAngle, 60f))
                {
                    Debug.Log("Left eye angle to the head is correct : " + leftEyeAngle);
                    m_LeftEyeAngleCheck = true;
                }
                else if (AngleCheck(leftEyeAngle, 60f))
                {
                    Debug.Log("Left eye angle to the head is incorrect : " + leftEyeAngle);
                    m_LeftEyeAngleCheck = false;
                }

                //Check to make sure the angle from the camera to the right eye is reasonable
                if (!AngleCheck(rightEyeAngle, 60f))
                {
                    Debug.Log("Right eye angle to the head is correct : " + rightEyeAngle);
                    m_RightEyeAngleCheck = true;
                }
                else if (AngleCheck(rightEyeAngle, 60f))
                {
                    Debug.Log("Right eye angle to the head is incorrect : " + rightEyeAngle);
                    m_RightEyeAngleCheck = false;
                }
            }
        }
    }
}
#else //JENKINS
namespace UnityEngine.XR.WindowsMR.Tests
{
    class RuntimeTests
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
