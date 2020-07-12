using System;
using System.IO;
using System.Collections.Generic;
using UnityEngine;
using LoggingModule;
using AsyncSerializer;
using System.Threading.Tasks;

namespace SaveAndLoad
{
    public class SaveAndLoadModule : MonoBehaviour
    {
        static string sessionPath = Application.persistentDataPath + "/session.data";
        static string baseDataPath = Application.streamingAssetsPath + "/BaseData/session.data";

        public static List<BrowserData> SerializeBrowsers(List<Browser> browsers)
        {
            List<BrowserData> browserList = new List<BrowserData>();
            foreach (Browser browser in browsers)
            {
                Vector3 pos = browser.transform.position;
                Vector3 fwd = browser.transform.forward;

                BrowserPackage lastPackage = browser.package_list.last_selected as BrowserPackage;
                string lastPackageName = lastPackage == null ? "" : lastPackage.name;

                Transform lastClass = browser.class_list.Find(lastPackageName);
                string lastClassName = lastClass == null ?
                    "" :
                    lastClass.gameObject.GetComponent<ClassWindow>().last_selected.name;

                string lastSideName = browser.lastSelectedSide;

                browserList.Add(new BrowserData(pos, fwd, lastClassName, lastPackageName, lastSideName));
            }
            return browserList;
        }

        public static void DeserializeBrowsers(Session session, VRIDEController player)
        {
            List<BrowserData> browsersData = session.browsers;
            List<Browser> browsers = new List<Browser>();

            foreach (BrowserData bdata in browsersData)
            {
                Vector3 pos = new Vector3(bdata.position.x, 0f, bdata.position.z);
                Vector3 fwd = new Vector3(bdata.forward.x, bdata.forward.y, bdata.forward.z);
                Vector3 final_pos = new Vector3(bdata.position.x, 2.25f, bdata.position.z);

                Browser browser = Instantiator.Instance.Browser();
                browser.Initialize(pos, final_pos, fwd, player);
                Transform lsp = browser.package_list.transform.Find(bdata.lastSelectedPackage);
                if (lsp != null && bdata.lastSelectedPackage != "")
                {
                    lsp.gameObject.GetComponent<BrowserPackage>().click();
                    Transform lsc = browser.class_list.Find(bdata.lastSelectedPackage).Find(bdata.lastSelectedClass);
                    if (lsc != null && bdata.lastSelectedClass != "")
                    {
                        lsc.gameObject.GetComponent<BrowserClass>().click();
                    }
                }
                if (bdata.lastSelectedSide == "ClassSide")
                    browser.onSelectClassSide();
                else
                    browser.onSelectInstanceSide();

                browsers.Add(browser);
                InteractionLogger.Count("Browser");
            }
            player.browsers = browsers;
        }

        public static List<PlaygroundData> SerializePlaygrounds(List<Playground> playgrounds)
        {
            List<PlaygroundData> playgroundList = new List<PlaygroundData>();
            foreach (Playground playground in playgrounds)
            {
                Vector3 pos = playground.transform.position;
                Vector3 fwd = playground.transform.forward;
                string sourceCode = playground.field.text;
                playgroundList.Add(new PlaygroundData(pos, fwd, sourceCode));
            }
            return playgroundList;
        }

        public static void DeserializePlaygrounds(Session session, VRIDEController player)
        {
            List<PlaygroundData> playgroundsData = session.playgrounds;
            List<Playground> playgrounds = new List<Playground>();
            foreach (PlaygroundData pdata in playgroundsData)
            {
                Vector3 pos = new Vector3(pdata.position.x, 0f, pdata.position.z);
                Vector3 fwd = new Vector3(pdata.forward.x, pdata.forward.y, pdata.forward.z);
                Vector3 final_pos = new Vector3(pdata.position.x, 2f, pdata.position.z);

                Playground playground = Instantiator.Instance.Playground();
                playground.Initialize(pos, final_pos, fwd, player);
                playground.field.text = pdata.sourceCode;
                playgrounds.Add(playground);
                InteractionLogger.Count("Playground");
            }
            player.playgrounds = playgrounds;
        }

        public static List<InspectorData> SerializeInspectors(List<Inspector> inspectors)
        {
            List<InspectorData> inspectorList = new List<InspectorData>();
            foreach (Inspector inspector in inspectors)
            {
                Vector3 pos = inspector.transform.position;
                Vector3 fwd = inspector.transform.forward;
                string rows = inspector.data;
                inspectorList.Add(new InspectorData(pos, fwd, rows));
            }
            return inspectorList;
        }

        public static void DeserializeInspectors(Session session, VRIDEController player)
        {
            List<InspectorData> inspectorsData = session.inspectors;
            List<Inspector> inspectors = new List<Inspector>();
            foreach (InspectorData idata in inspectorsData)
            {
                Vector3 pos = new Vector3(idata.position.x, 0f, idata.position.z);
                Vector3 fwd = new Vector3(idata.forward.x, idata.forward.y, idata.forward.z);
                Vector3 final_pos = new Vector3(idata.position.x, 2f, idata.position.z);

                Inspector inspector = Instantiator.Instance.Inspector();
                inspector.setContent(idata.rows);
                inspector.Initialize(pos, final_pos, fwd, player);

                inspectors.Add(inspector);

                InteractionLogger.Count("Inspector");
            }
            player.inspectors = inspectors;
        }

        public static List<SVGData> SerializeGraphs(List<Graph> svgs)
        {
            List<SVGData> graphs = new List<SVGData>();
            foreach (Graph graph in svgs)
            {
                Vector3 pos = graph.transform.position;
                Vector3 fwd = graph.transform.forward;
                string raw_image = graph.raw_image;
                string type = graph.type;
                graphs.Add(new SVGData(pos, fwd, raw_image, type));
            }
            return graphs;
        }

        public static void DeserializeGraphs(Session session, VRIDEController player)
        {
            List<SVGData> graphsData = session.graphs;
            List<Graph> graphs = new List<Graph>();
            foreach (SVGData gdata in graphsData)
            {
                Vector3 pos = new Vector3(gdata.position.x, 0f, gdata.position.z);
                Vector3 fwd = new Vector3(gdata.forward.x, gdata.forward.y, gdata.forward.z);
                Vector3 final_pos = new Vector3(gdata.position.x, 2f, gdata.position.z);

                string rawImage = gdata.rawImage;
                string type = gdata.type;

                Graph graph = Instantiator.Instance.Graph();
                graph.setSprite(rawImage, type);
                graph.Initialize(pos, final_pos, fwd, player);
                graphs.Add(graph);

                InteractionLogger.Count("GraphObject");
            }
            player.graphs = graphs;
        }

        public static async Task Save(VRIDEController player)
        {
            Session s = new Session(
                VRIDEController.sysData,
                SerializeBrowsers(player.browsers),
                SerializePlaygrounds(player.playgrounds),
                SerializeInspectors(player.inspectors),
                SerializeGraphs(player.graphs)
            );
            await AsynchronousSerializer.Serialize(sessionPath, s);
        }

        public static async Task Load(VRIDEController player)
        {
            if (File.Exists(sessionPath))
            {
                Session session = await AsynchronousSerializer.Deserialize(sessionPath);

                VRIDEController.sysData = session.classesAndMethods;
                DeserializeBrowsers(session, player);
                DeserializePlaygrounds(session, player);
                DeserializeInspectors(session, player);
                DeserializeGraphs(session, player);
            }
            else
            {
                Session session = await AsynchronousSerializer.Deserialize(baseDataPath);

                VRIDEController.sysData = session.classesAndMethods;
                player.browsers = new List<Browser>();
                player.playgrounds = new List<Playground>();
                player.inspectors = new List<Inspector>();
                player.graphs = new List<Graph>();
            }
        }
    }
}