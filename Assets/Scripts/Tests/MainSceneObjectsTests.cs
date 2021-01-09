using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.UI;
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

            GameObject browser = GameObject.Instantiate(browserPrefab);
            yield return null;
            Button b1 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button").GetComponent<Button>();
            Button b2 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (1)").GetComponent<Button>();
            Button b3 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (7)").GetComponent<Button>();
            Button b4 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (6)").GetComponent<Button>();
            Button b5 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (8)").GetComponent<Button>();
            Button b6 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (9)").GetComponent<Button>();
            Button b7 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (10)").GetComponent<Button>();
            Button b8 = browser.transform.Find("Panel/Panel (1)/Toolbar/Button (11)").GetComponent<Button>();
            yield return null;
            Assert.IsTrue(b1.isActiveAndEnabled && b1.interactable);
            Assert.IsTrue(b2.isActiveAndEnabled && b2.interactable);
            Assert.IsTrue(b3.isActiveAndEnabled && b3.interactable);
            Assert.IsTrue(b4.isActiveAndEnabled && b4.interactable);
            Assert.IsTrue(b5.isActiveAndEnabled && b5.interactable);
            Assert.IsTrue(b6.isActiveAndEnabled && b6.interactable);
            Assert.IsTrue(b7.isActiveAndEnabled && b7.interactable);
            Assert.IsTrue(b8.isActiveAndEnabled && b8.interactable);

            GameObject playground = GameObject.Instantiate(playgroundPrefab);
            yield return null;
            b1 = playground.transform.Find("Panel/Toolbar/Button").GetComponent<Button>();
            b2 = playground.transform.Find("Panel/Toolbar/Button (1)").GetComponent<Button>();
            b3 = playground.transform.Find("Panel/Toolbar/Button (2)").GetComponent<Button>();
            b4 = playground.transform.Find("Panel/Toolbar/Button (3)").GetComponent<Button>();
            b5 = playground.transform.Find("Panel/Toolbar/Button (4)").GetComponent<Button>();
            b6 = playground.transform.Find("Panel/Toolbar/Button (5)").GetComponent<Button>();
            b7 = playground.transform.Find("Panel/Toolbar/Button (6)").GetComponent<Button>();
            b8 = playground.transform.Find("Panel/Toolbar/Button (7)").GetComponent<Button>();
            yield return null;
            Assert.IsTrue(b1.isActiveAndEnabled && b1.interactable);
            Assert.IsTrue(b2.isActiveAndEnabled && b2.interactable);
            Assert.IsTrue(b3.isActiveAndEnabled && b3.interactable);
            Assert.IsTrue(b4.isActiveAndEnabled && b4.interactable);
            Assert.IsTrue(b5.isActiveAndEnabled && b5.interactable);
            Assert.IsTrue(b6.isActiveAndEnabled && b6.interactable);
            Assert.IsTrue(b7.isActiveAndEnabled && b7.interactable);
            Assert.IsTrue(b8.isActiveAndEnabled && b8.interactable);
        }
    }
}