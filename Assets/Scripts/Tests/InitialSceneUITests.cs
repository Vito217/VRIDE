using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.SceneManagement;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using TestProperties;

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

        // A Test behaves as an ordinary method
        [Test]
        public void InitialSceneUITestsScriptSimplePasses()
        {
            // Use the Assert class to test conditions

            Assert.IsTrue(SceneManager.GetActiveScene().name == "InitialScene");
        }

        // A UnityTest behaves like a coroutine in Play Mode. In Edit Mode you can use
        // 'yield return null;' to skip a frame.
        [UnityTest]
        public IEnumerator InitialSceneUITestsScriptWithEnumeratorPasses()
        {
            // Use the Assert class to test conditions.
            // Use yield to skip a frame.

            // Preparing test

            List<GameObject> objs = new List<GameObject>() { 
                submitButton, exitButton, ipField, portField, inputsBar, infoBar
            };

            foreach(GameObject ob in objs)
            {
                ob.AddComponent<GeneralTestProperties>();
                ob.AddComponent<EventTrigger>();
                EventTrigger.Entry entry = new EventTrigger.Entry();
                EventTrigger.Entry entryTwo = new EventTrigger.Entry();
                entry.eventID = EventTriggerType.PointerDown;
                entryTwo.eventID = EventTriggerType.PointerUp;
                entry.callback.AddListener((data) => { ob.AddComponent<GeneralTestProperties>().OnPointerEnter(); });
                entryTwo.callback.AddListener((data) => { ob.AddComponent<GeneralTestProperties>().OnPointerExit(); });
                ob.AddComponent<EventTrigger>().triggers.Add(entry);
                ob.AddComponent<EventTrigger>().triggers.Add(entryTwo);
            }

            // Testing UI interactivity
            Transform rightPointer = user.transform.Find("VivePointers/Right");
            Transform leftPointer = user.transform.Find("VivePointers/Left");

            rightPointer.LookAt(ipField.transform);
            yield return null;
            Assert.IsTrue(ipField.GetComponent<GeneralTestProperties>().IsHovered());

            rightPointer.LookAt(portField.transform);
            yield return null;
            Assert.IsTrue(portField.GetComponent<GeneralTestProperties>().IsHovered());

            rightPointer.LookAt(submitButton.transform);
            yield return null;
            Assert.IsTrue(submitButton.GetComponent<GeneralTestProperties>().IsHovered());
        
            
        }
    }
}