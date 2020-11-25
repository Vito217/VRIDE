#if JENKINS
using NUnit.Framework;
using UnityEngine;
using UnityEngine.SpatialTracking;
using UnityEngine.TestTools;

#if UNITY_EDITOR
using UnityEditor;
#endif

[PrebuildSetup(typeof(TestPrebuildSetup))]
public class TestBaseSetup
{
    public static GameObject m_Camera;
    public static GameObject m_Light;
    public static GameObject m_Cube;

    public static GameObject m_XrManager;
    public static GameObject m_TrackingRig;
    public static TrackedPoseDriver m_TrackHead;

    public TestSetupHelpers m_TestSetupHelpers;

    [SetUp]
    public void XrSdkTestBaseSetup()
    {
        m_Cube = new GameObject("Cube");
        m_TestSetupHelpers = new TestSetupHelpers();

        m_TestSetupHelpers.TestStageSetup(TestStageConfig.BaseStageSetup);
    }

    [TearDown]
    public void XrSdkTestBaseTearDown()
    {
        m_TestSetupHelpers.TestStageSetup(TestStageConfig.CleanStage);
    }

    public class TestPrebuildSetup : IPrebuildSetup
    {
        public void Setup()
        {
#if UNITY_EDITOR
            // Configure StandAlone build
            EditorUserBuildSettings.SwitchActiveBuildTarget(BuildTargetGroup.Standalone, BuildTarget.StandaloneWindows64);
            EditorUserBuildSettings.wsaUWPBuildType = WSAUWPBuildType.D3D;
            EditorUserBuildSettings.wsaSubtarget = WSASubtarget.AnyDevice;
            EditorUserBuildSettings.allowDebugging = true;

            PlayerSettings.SetScriptingBackend(BuildTargetGroup.Standalone, ScriptingImplementation.IL2CPP);
#endif
        }
    }
}
#endif //JENKINS
