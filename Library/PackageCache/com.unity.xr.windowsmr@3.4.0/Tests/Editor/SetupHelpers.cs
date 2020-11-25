#if JENKINS
using UnityEngine;
using UnityEngine.SpatialTracking;

#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.XR.Management;
#endif

public enum TestStageConfig
{
    BaseStageSetup,
    CleanStage,
    MultiPass,
    Instancing
}

public enum TestCubesConfig
{
    None,
    TestCube,
    PerformanceMassFloorObjects,
    PerformanceMassObjects,
    TestMassCube
}

public class TestSetupHelpers : TestBaseSetup
{
    private int m_CubeCount = 0;

    public void CreateXRGameObjects()
    {
        m_TrackingRig = GameObject.Instantiate(Resources.Load("TestSetup/TrackingRig")) as GameObject;
    }

    public void CameraLightSetup()
    {
        m_Camera = new GameObject("Main Camera");
        m_Camera.tag = "MainCamera";
        m_Camera.AddComponent<Camera>();
        m_Camera.AddComponent<TrackedPoseDriver>();
        m_TrackHead = m_Camera.GetComponent<TrackedPoseDriver>();
        m_TrackHead.SetPoseSource(TrackedPoseDriver.DeviceType.GenericXRDevice, TrackedPoseDriver.TrackedPose.Center);

        m_Light = new GameObject("Light");
        Light light = m_Light.AddComponent<Light>();
        light.type = LightType.Directional;
    }

    public void TestCubeCreation()
    {
        m_Cube = GameObject.CreatePrimitive(PrimitiveType.Cube);
        m_Cube.transform.position = 5f * Vector3.forward;
    }

    public void CreateMassFloorObjects()
    {
        float x = -3.0f;
        float y = -0.5f;
        float zRow1 = 2.0f;
        float zRow2 = 2.0f;
        float zRow3 = 2.0f;
        float zRow4 = 2.0f;

        for (int i = 0; i < 20; i++)
        {
            var obj = GameObject.CreatePrimitive(PrimitiveType.Cube);
            m_CubeCount += 1;
            obj.name = "TestCube " + m_CubeCount;
            obj.transform.localScale = new Vector3(0.1f, 0.1f, 0.1f);
            obj.transform.localPosition = new Vector3(x, y, zRow1);

            zRow1 = zRow1 + 0.5f;
            x = -2f;
        }

        for (int i = 0; i < 20; i++)
        {
            var obj = GameObject.CreatePrimitive(PrimitiveType.Cube);
            m_CubeCount += 1;
            obj.name = "TestCube " + m_CubeCount;
            obj.transform.localScale = new Vector3(0.1f, 0.1f, 0.1f);
            obj.transform.localPosition = new Vector3(x, y, zRow2);

            zRow2 = zRow2 + 0.5f;
            x = -1f;
        }

        for (int i = 0; i < 20; i++)
        {
            var obj = GameObject.CreatePrimitive(PrimitiveType.Cube);
            m_CubeCount += 1;
            obj.name = "TestCube " + m_CubeCount;
            obj.transform.localScale = new Vector3(0.1f, 0.1f, 0.1f);
            obj.transform.localPosition = new Vector3(x, y, zRow3);

            zRow3 = zRow3 + 0.5f;
            x = 0f;
        }

        for (int i = 0; i < 20; i++)
        {
            var obj = GameObject.CreatePrimitive(PrimitiveType.Cube);
            m_CubeCount += 1;
            obj.name = "TestCube " + m_CubeCount;
            obj.transform.localScale = new Vector3(0.1f, 0.1f, 0.1f);
            obj.transform.localPosition = new Vector3(x, y, zRow4);

            zRow4 = zRow4 + 0.5f;
            x = 1f;
        }
    }

    public void CreateMassObjects()
    {
        float xRow1 = -0.5f;
        float xRow2 = -0.5f;
        float xRow3 = -0.5f;
        float yRow1 = 0.2f;
        float yRow2 = -0.2f;
        float yRow3 = -0.01f;
        float zRow1 = 2.5f;
        float zRow2 = 2.3f;

        for (int i = 0; i < 17; i++)
        {
            var obj = GameObject.CreatePrimitive(PrimitiveType.Cube);
            m_CubeCount += 1;
            obj.transform.localScale = new Vector3(0.1f, 0.1f, 0.1f);
            obj.transform.localPosition = new Vector3(xRow1, yRow1, zRow1);
            obj.name = "TestCube " + m_CubeCount;

            if (i < 5)
            {
                xRow1 = xRow1 + 0.2f;
                obj.transform.localPosition = new Vector3(xRow1, yRow1, zRow1);
            }
            else if (i > 5 && i < 11)
            {
                xRow2 = xRow2 + 0.2f;
                obj.transform.localPosition = new Vector3(xRow2, yRow2, zRow1);
            }
            else if (i > 11)
            {
                xRow3 = xRow3 + 0.2f;
                obj.transform.localPosition = new Vector3(xRow3, yRow3, zRow2);
            }
        }
    }

    public void CleanUpTestCubes()
    {
        if (m_CubeCount > 0)
        {
            for (int i = 0; i < m_CubeCount + 1; i++)
            {
                var obj = GameObject.Find("TestCube " + i);
                Object.DestroyImmediate(obj);
            }
        }

        if (GameObject.Find("Cube"))
        {
            Object.DestroyImmediate(GameObject.Find("Cube"));
        }
    }

    private void DestroyGameObject(ref GameObject obj)
    {
        if (obj != null)
        {
            Object.DestroyImmediate(obj);
            obj = null;
        }
    }

    public void CleanUpCamerLights()
    {
        DestroyGameObject(ref m_Camera);
        DestroyGameObject(ref m_Light);
    }

    public void CleanUpXRGameObjects()
    {
        DestroyGameObject(ref m_XrManager);
        DestroyGameObject(ref m_TrackingRig);
    }

#if UNITY_EDITOR
    public void EnsureMultiPassRendering() => PlayerSettings.stereoRenderingPath = StereoRenderingPath.MultiPass;

    public void EnsureInstancingRendering() => PlayerSettings.stereoRenderingPath = StereoRenderingPath.Instancing;
#endif

    public void TestStageSetup(TestStageConfig TestConfiguration)
    {
        switch (TestConfiguration)
        {
            case TestStageConfig.BaseStageSetup:
                    CreateXRGameObjects();
                    CameraLightSetup();
                    break;

            case TestStageConfig.CleanStage:
                    CleanUpCamerLights();
                    CleanUpTestCubes();
                    CleanUpXRGameObjects();
#if UNITY_EDITOR
                    EnsureInstancingRendering();
#endif
                break;
#if UNITY_EDITOR
            case TestStageConfig.Instancing:
                    EnsureInstancingRendering();
                    break;

                case TestStageConfig.MultiPass:
                    EnsureMultiPassRendering();
                    break;
#endif
        }
    }

    public void TestCubeSetup(TestCubesConfig TestConfiguration)
    {
        switch (TestConfiguration)
        {
            case TestCubesConfig.TestCube:
                TestCubeCreation();
                break;

            case TestCubesConfig.PerformanceMassFloorObjects:
                CreateMassFloorObjects();
                break;

            case TestCubesConfig.PerformanceMassObjects:
                CreateMassObjects();
                break;

            case TestCubesConfig.None:
                break;
        }
    }
}
#endif //JENKINS
