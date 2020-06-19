using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Versioning;
using System.Security.Cryptography;
using System.Runtime.Serialization.Formatters.Binary;
using UnityEngine;
using UnityEngine.Events;
using UnityEngine.UI;
using TMPro;

namespace SaveAndLoad
{
    public static class SaveAndLoadModule
    {
        static string prefabs = "Prefabs/3.0/UI/";
        static string editorPath = "Editor/Panel/InputField (TMP)";
        static string classPath = "Classes/Panel/Scroll View/Viewport/Content";
        static string methodPath = "Methods/Panel/Scroll View/Viewport/Content/";
        static string inspectedPath = "InspectorTable/Panel/Scroll View/Viewport/Content";
        static string sessionPath = Application.persistentDataPath + "/session.data";

        public static BrowserInit browserPrefab = Resources.Load<BrowserInit>(prefabs + "Browser");
        public static PlaygroundInit playgroundPrefab = Resources.Load<PlaygroundInit>(prefabs + "Playground");
        public static InspectorInit inspectorPrefab = Resources.Load<InspectorInit>(prefabs + "Inspector");
        public static SVGObjectInit svgPrefab = Resources.Load<SVGObjectInit>(prefabs + "GraphObject");

        public static SystemData SerializeSystemData(GameObject browser)
        {
            List<Tuple<string, string>> classes = new List<Tuple<string, string>>();
            Dictionary<string, List<Tuple<string, string>>> methodLists =
                new Dictionary<string, List<Tuple<string, string>>>();

            Transform classList = browser.transform.Find(classPath);
            foreach(Transform classObject in classList)
            {
                string className = classObject.gameObject.name;
                string sourceCode = classObject.gameObject.GetComponent<BrowserClass>().sourceCode;
                classes.Add(new Tuple<string, string>(className, sourceCode));

                Transform methodList = browser.transform.Find(methodPath + className);
                List<Tuple<string, string>> methods = new List<Tuple<string, string>>();
                foreach (Transform methodObject in methodList)
                {
                    string methodName = methodObject.gameObject.name;
                    string code = methodObject.gameObject.GetComponent<BrowserMethod>().sourceCode;
                    methods.Add(new Tuple<string, string>(methodName, code));
                }

                methodLists.Add(className, methods);
            }

            return new SystemData(classes, methodLists);
        }

        public static List<BrowserData> SerializeBrowsers(List<GameObject> browsers)
        {
            List<BrowserData> browserList = new List<BrowserData>();
            foreach(GameObject browser in browsers)
            {
                Vector3 pos = browser.transform.position; Debug.Log(pos.x + "," + pos.z);
                Vector3 fwd = browser.transform.forward;
                BrowserClass lastClass = browser.transform.Find(classPath)
                    .gameObject.GetComponent<ClassWindow>().last_selected_class;
                string lastClassName = lastClass == null ? "" : lastClass.name;
                browserList.Add(new BrowserData(pos, fwd, lastClassName));
            }
            return browserList;
        }

        public static void DeserializeBrowsers(Session session, VRIDEController player)
        {
            SystemData data = session.classesAndMethods;
            List<BrowserData> browsersData = session.browsers;
            List<GameObject> browsers = new List<GameObject>();
            foreach(BrowserData bdata in browsersData)
            {
                Vector3 pos = new Vector3(bdata.position.x, 0f, bdata.position.z);
                Vector3 final_pos = new Vector3(bdata.position.x, 2f, bdata.position.z);
                Vector3 fwd = new Vector3(bdata.forward.x, bdata.forward.y, bdata.forward.z);
                BrowserInit browser = UnityEngine.Object.Instantiate(browserPrefab);
                browser.Initialize(pos, final_pos, fwd, player.gameObject);
                browsers.Add(browser.gameObject);
            }
            player.browsers = browsers;
        }

        public static List<PlaygroundData> SerializePlaygrounds(List<GameObject> playgrounds)
        {
            List<PlaygroundData> playgroundList = new List<PlaygroundData>();
            foreach (GameObject playground in playgrounds)
            {
                Vector3 pos = playground.transform.position;
                Vector3 fwd = playground.transform.forward;
                string sourceCode = playground.transform.Find(editorPath)
                    .gameObject.GetComponent<TMP_InputField>().text;
                playgroundList.Add(new PlaygroundData(pos, fwd, sourceCode));
            }
            return playgroundList;
        }

        public static List<InspectorData> SerializeInspectors(List<GameObject> inspectors)
        {
            List<InspectorData> inspectorList = new List<InspectorData>();
            foreach (GameObject inspector in inspectors)
            {
                Vector3 pos = inspector.transform.position;
                Vector3 fwd = inspector.transform.forward;
                List<Tuple<string, string>> rowList = new List<Tuple<string, string>>();
                Transform rows = inspector.transform.Find(inspectedPath);
                foreach(Transform row in rows)
                {
                    string variable = row.Find("Variable").Find("Text (TMP)")
                        .gameObject.GetComponent<TextMeshProUGUI>().text;
                    string value = row.Find("Value").Find("Text (TMP)")
                        .gameObject.GetComponent<TextMeshProUGUI>().text;
                    rowList.Add(new Tuple<string, string>(variable, value));
                }
                inspectorList.Add(new InspectorData(pos, fwd, rowList));
            }
            return inspectorList;
        }

        public static List<SVGData> SerializeSVGs(List<GameObject> svgs)
        {
            List<SVGData> graphs = new List<SVGData>();
            foreach (GameObject graph in svgs)
            {
                Vector3 pos = graph.transform.position;
                Vector3 fwd = graph.transform.forward;
                SVGObjectInit cmp = graph.GetComponent<SVGObjectInit>();
                string raw_image = cmp.raw_image;
                string type = cmp.type;
                graphs.Add(new SVGData(pos, fwd, raw_image, type));
            }
            return graphs;
        }

        public static void Save(VRIDEController player)
        {
            Session s = new Session(
                SerializeSystemData(player.og_browser.gameObject),
                SerializeBrowsers(player.browsers),
                SerializePlaygrounds(player.playgrounds),
                SerializeInspectors(player.inspectors),
                SerializeSVGs(player.graphs)
            );
            if (File.Exists(sessionPath)) File.Delete(sessionPath);
            BinaryFormatter bf = new BinaryFormatter();
            FileStream file = File.Create(sessionPath);
            bf.Serialize(file, s);
            file.Close();
        }

        public static void Load(VRIDEController player)
        {
            if (File.Exists(sessionPath))
            {
                BinaryFormatter bf = new BinaryFormatter();
                FileStream file = File.Open(sessionPath, FileMode.Open);
                Session session = (Session) bf.Deserialize(file);
                file.Close();

                DeserializeBrowsers(session, player);
            }
        }
    }
}
