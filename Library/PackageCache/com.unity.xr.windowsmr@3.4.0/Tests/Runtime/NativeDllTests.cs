#if JENKINS
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using UnityEngine;

namespace UnityEngine.XR.WindowsMR.Tests
{
    class DllTests
    {
        internal class NativeDllTests : TestBaseSetup
        {
            private bool m_SceneObjectsLoaded = false;
            private bool m_RenderingImage = false;

            private GameObject m_RenderPlain;
            private GameObject m_BaseSphere;

            private Light m_SpotLight;

            private RenderTexture m_CurrentRenderTexture;

            private int m_NonPerformantFrameCount;

            [SetUp]
            public void NativeDllTestSetUp()
            {
                CreateDllLoad();
            }

            [TearDown]
            public void NativeDllTestTearDown()
            {
                CleanUpTestsObjects();
            }

            private void CreateDllLoad()
            {
                m_RenderPlain = Object.Instantiate(Resources.Load("Prefabs/_PlaneThatCallsIntoPlugin", typeof(GameObject)) as GameObject);
                m_SpotLight = Object.Instantiate(Resources.Load("Prefabs/Spotlight", typeof(Light)) as Light);

                m_SceneObjectsLoaded = true;
            }

            private void CreatePerfSphere()
            {
                m_BaseSphere = Object.Instantiate(Resources.Load("Prefabs/Sphere", typeof(GameObject)) as GameObject);
            }

            private void CleanUpTestsObjects()
            {
                Object.Destroy(m_RenderPlain);
                if (m_BaseSphere != null)
                {
                    Object.Destroy(m_BaseSphere);
                }

                Object.Destroy(m_SpotLight);

                if (GameObject.Find("Spotlight(Clone)"))
                {
                    Object.Destroy(GameObject.Find("Spotlight(Clone)"));
                }
            }

            [UnityTest]
            public IEnumerator NativeDllSceneBuild()
            {
                yield return null;
                Assert.IsNotNull(m_SceneObjectsLoaded, "Scene Objects was not created");
            }

            [UnityTest]
            public IEnumerator NativeDllTest()
            {
                yield return new WaitForSeconds(1);
                m_RenderingImage = IsPlaneRendering();
                Assert.IsTrue(m_RenderingImage, "Image rendering couldn't be found");
            }

            // Current Bug where start up is causing fps slow down due to spin up [1115410]
            [UnityTest]
            public IEnumerator RenderingFPSCheck()
            {
                yield return new WaitForSeconds(3f);
                CreatePerfSphere();
                m_NonPerformantFrameCount = m_BaseSphere.GetComponent<FpsMeasure>().m_NonPerformantFrameCount;
                yield return new WaitForSeconds(10f);
                Assert.AreEqual(0, m_NonPerformantFrameCount, "Failed to keep every frame inside the target frame time for the tested window");
            }

            public bool IsPlaneRendering()
            {
                bool filter = false;
                bool textsize = false;

                if (m_RenderPlain.GetComponent<Renderer>().material.mainTexture.filterMode == FilterMode.Point)
                {
                    filter = true;
                }

                if (m_RenderPlain.GetComponent<Renderer>().material.mainTexture.height == 256 && m_RenderPlain.GetComponent<Renderer>().material.mainTexture.width == 256)
                {
                    textsize = true;
                }

                if (filter && textsize)
                {
                    return true;
                }

                return false;
            }
        }
    }
}
#endif //JENKINS
