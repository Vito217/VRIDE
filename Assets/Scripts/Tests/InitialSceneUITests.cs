using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.TestTools;
using UnityEngine.SceneManagement;
using TMPro;

namespace Tests
{
    public class InitialSceneUITests
    {
        // UI Elements
        public GameObject submitButton;
        public GameObject exitButton;
        public GameObject ipField;
        public GameObject portField;
        public GameObject inputsBar;
        public GameObject infoBar;
        public GameObject inputsWindow;
        public GameObject infoWindow;

        // The user
        public GameObject user;

        [UnityTest]
        public IEnumerator InitialSceneUITestsScriptWithEnumeratorPasses()
        {
            SceneManager.LoadScene("InitialScene", LoadSceneMode.Single);
            yield return null;
            Assert.IsTrue(SceneManager.GetActiveScene().name == "InitialScene");

            submitButton = GameObject.Find("TitleScreen/Panel/Enter Button");
            yield return null;
            Assert.IsNotNull(submitButton);

            exitButton = GameObject.Find("TitleScreen/Panel/Exit Button");
            yield return null;
            Assert.IsNotNull(exitButton);

            ipField = GameObject.Find("TitleScreen/Panel/InputField (TMP)");
            yield return null;
            Assert.IsNotNull(ipField);

            portField = GameObject.Find("TitleScreen/Panel/InputField (TMP) (1)");
            yield return null;
            Assert.IsNotNull(portField);

            inputsBar = GameObject.Find("TitleScreen/Panel/Bar");
            yield return null;
            Assert.IsNotNull(inputsBar);

            infoBar = GameObject.Find("Canvas/Panel/Button (1)");
            yield return null;
            Assert.IsNotNull(infoBar);

            user = GameObject.Find("VRPlayer");
            yield return null;
            Assert.IsNotNull(user);

            // Testing UI interactivity
            Transform rightPointer = user.transform.Find("VivePointers/Right");
            Transform leftPointer = user.transform.Find("VivePointers/Left");
            yield return null;

            Button sb = submitButton.GetComponent<Button>();
            Assert.IsTrue(sb.isActiveAndEnabled && sb.interactable);

            Button eb = exitButton.GetComponent<Button>();
            Assert.IsTrue(eb.isActiveAndEnabled && eb.interactable);

            Button inpb = inputsBar.GetComponent<Button>();
            Assert.IsTrue(inpb.isActiveAndEnabled && inpb.interactable);

            Button infb = infoBar.GetComponent<Button>();
            Assert.IsTrue(infb.isActiveAndEnabled && infb.interactable);

            TMP_InputField ip = ipField.GetComponent<TMP_InputField>();
            Assert.IsTrue(ip.isActiveAndEnabled && ip.interactable);

            TMP_InputField port = portField.GetComponent<TMP_InputField>();
            Assert.IsTrue(port.isActiveAndEnabled && port.interactable);
        }
    }
}