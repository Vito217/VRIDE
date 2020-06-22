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
using InstantiatorModule;

namespace SaveAndLoad
{
    public static class SaveAndLoadModule
    {
        static string sessionPath = Application.persistentDataPath + "/session.data";

        public static List<BrowserData> SerializeBrowsers(List<GameObject> browsers)
        {
            List<BrowserData> browserList = new List<BrowserData>();
            foreach(GameObject browser in browsers)
            {
                Vector3 pos = browser.transform.position;
                Vector3 fwd = browser.transform.forward;
                BrowserClass lastClass = browser.transform.Find(Instantiator.classPath)
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
                Vector3 fwd = new Vector3(bdata.forward.x, bdata.forward.y, bdata.forward.z);
                Vector3 final_pos = new Vector3(bdata.position.x, 2f, bdata.position.z);

                BrowserInit browser = Instantiator.Browser(data) as BrowserInit;
                browser.Initialize(pos, final_pos, fwd, player.gameObject);
                browsers.Add(browser.gameObject);

                Transform lsc = browser.transform.Find(Instantiator.classPath).Find(bdata.lastSelectedClass);
                if (lsc) lsc.gameObject.GetComponent<BrowserClass>().click();
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
                string sourceCode = playground.transform.Find(Instantiator.editorPath)
                    .gameObject.GetComponent<TMP_InputField>().text;
                playgroundList.Add(new PlaygroundData(pos, fwd, sourceCode));
            }
            return playgroundList;
        }

        public static void DeserializePlaygrounds(Session session, VRIDEController player)
        {
            List<PlaygroundData> playgroundsData = session.playgrounds;
            List<GameObject> playgrounds = new List<GameObject>();
            foreach(PlaygroundData pdata in playgroundsData)
            {
                Vector3 pos = new Vector3(pdata.position.x, 0f, pdata.position.z);
                Vector3 fwd = new Vector3(pdata.forward.x, pdata.forward.y, pdata.forward.z);
                Vector3 final_pos = new Vector3(pdata.position.x, 2f, pdata.position.z);

                PlaygroundInit playground = Instantiator.Playground() as PlaygroundInit;
                playground.Initialize(pos, final_pos, fwd, player.gameObject);
                playground.transform.Find(Instantiator.editorPath).gameObject
                    .GetComponent<TMP_InputField>().text = pdata.sourceCode;

                playgrounds.Add(playground.gameObject);
            }
            player.playgrounds = playgrounds;
        }

        public static List<InspectorData> SerializeInspectors(List<GameObject> inspectors)
        {
            List<InspectorData> inspectorList = new List<InspectorData>();
            foreach (GameObject inspector in inspectors)
            {
                Vector3 pos = inspector.transform.position;
                Vector3 fwd = inspector.transform.forward;
                string rows = inspector.GetComponent<InspectorInit>().data;
                inspectorList.Add(new InspectorData(pos, fwd, rows));
            }
            return inspectorList;
        }

        public static void DeserializeInspectors(Session session, VRIDEController player)
        {
            List<InspectorData> inspectorsData = session.inspectors;
            List<GameObject> inspectors = new List<GameObject>();
            foreach (InspectorData idata in inspectorsData)
            {
                Vector3 pos = new Vector3(idata.position.x, 0f, idata.position.z);
                Vector3 fwd = new Vector3(idata.forward.x, idata.forward.y, idata.forward.z);
                Vector3 final_pos = new Vector3(idata.position.x, 2f, idata.position.z);

                InspectorInit inspector = Instantiator.Inspector() as InspectorInit;
                inspector.setContent(idata.rows);
                inspector.Initialize(pos, final_pos, fwd, player.gameObject);

                inspectors.Add(inspector.gameObject);
            }
            player.inspectors = inspectors;
        }

        public static List<SVGData> SerializeGraphs(List<GameObject> svgs)
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

        public static void DeserializeGraphs(Session session, VRIDEController player)
        {
            List<SVGData> graphsData = session.graphs;
            List<GameObject> graphs = new List<GameObject>();
            foreach(SVGData gdata in graphsData)
            {
                Vector3 pos = new Vector3(gdata.position.x, 0f, gdata.position.z);
                Vector3 fwd = new Vector3(gdata.forward.x, gdata.forward.y, gdata.forward.z);
                Vector3 final_pos = new Vector3(gdata.position.x, 2f, gdata.position.z);

                string rawImage = gdata.rawImage;
                string type = gdata.type;

                SVGObjectInit graph = Instantiator.Graph() as SVGObjectInit;
                graph.setSprite(rawImage, type);
                graph.Initialize(pos, final_pos, fwd, player.gameObject);

                graphs.Add(graph.gameObject);
            }
            player.graphs = graphs;
        }

        public static void Save(VRIDEController player)
        {
            Session s = new Session(
                VRIDEController.data,
                SerializeBrowsers(player.browsers),
                SerializePlaygrounds(player.playgrounds),
                SerializeInspectors(player.inspectors),
                SerializeGraphs(player.graphs)
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

                VRIDEController.data = session.classesAndMethods;
                DeserializeBrowsers(session, player);
                DeserializePlaygrounds(session, player);
                DeserializeInspectors(session, player);
                DeserializeGraphs(session, player);
            }
        }
    }
}
