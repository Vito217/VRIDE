using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.SceneManagement;

namespace Tests
{
    public class MainSceneObjectsTests
    {
        [UnityTest]
        public IEnumerator MainSceneObjectsTestsScriptWithEnumeratorPasses()
        {
            SceneManager.LoadScene("MainScene", LoadSceneMode.Single);
            yield return null;
            Assert.IsTrue(SceneManager.GetActiveScene().name == "MainScene");

            GameObject browserPrefab = Resources.Load<GameObject>("Prefabs/3.0/UI/Browser");
            GameObject playgroundPrefab = Resources.Load<GameObject>("Prefabs/3.0/UI/Playground");
            GameObject transcriptPrefab = Resources.Load<GameObject>("Prefabs/3.0/UI/Transcript");
            GameObject InspectorPrefab = Resources.Load<GameObject>("Prefabs/3.0/UI/Inspector");
            yield return null;

            Assert.IsNotNull(browserPrefab);
            Assert.IsNotNull(playgroundPrefab);
            Assert.IsNotNull(transcriptPrefab);
            Assert.IsNotNull(InspectorPrefab);
        }
    }
}