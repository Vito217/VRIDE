using System.IO;
using System.Collections.Generic;
using UnityEngine;
using LoggingModule;
using AsyncSerializer;
using System.Threading.Tasks;

/// <summary>
/// Tools for storing session data
/// </summary>
namespace SaveAndLoad
{
    /// <summary>
    /// Includes serializers and deserializers
    /// </summary>
    public class SaveAndLoadModule : MonoBehaviour
    {
        public static string sessionPath;
        
        public static string transcriptContents = "";
        public static List<Browser> browsers = new List<Browser>();
        public static List<Playground> playgrounds = new List<Playground>();
        public static List<Inspector> inspectors = new List<Inspector>();
        public static List<Graph> graphs = new List<Graph>();
        public static List<Transcript> transcripts = new List<Transcript>();

        /// <summary>
        /// Stores browsers data
        /// </summary>
        /// <returns>BrowserData list</returns>
        public static List<BrowserData> SerializeBrowsers()
        {
            List<BrowserData> browsersData = new List<BrowserData>();
            foreach (Browser browser in browsers)
            {
                Vector3 pos = browser.transform.position;
                Vector3 fwd = browser.transform.forward;

                string lastPackageName = "", lastClassName = "";
                BrowserPackage lastPackage = browser.package_list.last_selected as BrowserPackage;
                if(lastPackage != null)
                {
                    lastPackageName = lastPackage.name;
                    BrowserClass lastClass = browser.class_list.last_selected as BrowserClass;
                    if(lastClass != null) lastClassName = lastClass.name;
                }
                string lastSideName = browser.classSideToggle.isOn ? "ClassSide" : "InstanceSide";

                browsersData.Add(new BrowserData(pos, fwd, lastClassName, lastPackageName, lastSideName));
            }
            return browsersData;
        }

        /// <summary>
        /// Loads browsers data
        /// </summary>
        /// <param name="session">Previous session</param>
        public static void DeserializeBrowsers(Session session)
        {
            List<BrowserData> browsersData = session.browsers;

            foreach (BrowserData bdata in browsersData)
            {
                Browser browser = Instantiator.Instance.Browser();
                browser.transform.position = new Vector3(bdata.position.x, bdata.position.y, bdata.position.z);
                browser.transform.forward = new Vector3(bdata.forward.x, bdata.forward.y, bdata.forward.z);
                Transform lsp = browser.package_list.transform.Find(bdata.lastSelectedPackage);
                if (lsp != null && bdata.lastSelectedPackage != "")
                {
                    lsp.gameObject.GetComponent<BrowserPackage>().click();
                    BrowserClass lsc = browser.class_list.last_selected as BrowserClass;
                    if (lsc != null) lsc.click();
                }
                if (bdata.lastSelectedSide == "ClassSide") browser.onSelectClassSide();
                else browser.onSelectInstanceSide();

                browsers.Add(browser);
                InteractionLogger.Count("Browser", browser.GetInstanceID().ToString());
            }
        }

        /// <summary>
        /// Stores playgrounds data
        /// </summary>
        /// <returns>PlaygroundData list</returns>
        public static List<PlaygroundData> SerializePlaygrounds()
        {
            List<PlaygroundData> playgroundsData = new List<PlaygroundData>();
            foreach (Playground playground in playgrounds)
            {
                Vector3 pos = playground.transform.position;
                Vector3 fwd = playground.transform.forward;
                string sourceCode = playground.field.text;
                playgroundsData.Add(new PlaygroundData(pos, fwd, sourceCode));
            }
            return playgroundsData;
        }

        /// <summary>
        /// Loads playgrounds data
        /// </summary>
        /// <param name="session">Previous session</param>
        public static void DeserializePlaygrounds(Session session)
        {
            List<PlaygroundData> playgroundsData = session.playgrounds;

            foreach (PlaygroundData pdata in playgroundsData)
            {
                Playground playground = Instantiator.Instance.Playground();
                playground.transform.position = new Vector3(pdata.position.x, pdata.position.y, pdata.position.z);
                playground.transform.forward = new Vector3(pdata.forward.x, pdata.forward.y, pdata.forward.z);
                playground.field.text = pdata.sourceCode;
                playgrounds.Add(playground);
                InteractionLogger.Count("Playground", playground.GetInstanceID().ToString());
            }
        }

        /// <summary>
        /// Saves inspectors data
        /// </summary>
        /// <returns>InspectorData list</returns>
        public static List<InspectorData> SerializeInspectors()
        {
            List<InspectorData> inspectorsData = new List<InspectorData>();
            foreach (Inspector inspector in inspectors)
            {
                Vector3 pos = inspector.transform.position;
                Vector3 fwd = inspector.transform.forward;
                string rows = inspector.data;
                inspectorsData.Add(new InspectorData(pos, fwd, rows));
            }
            return inspectorsData;
        }

        /// <summary>
        /// Loads Inspectors data
        /// </summary>
        /// <param name="session">Previous session</param>
        public static void DeserializeInspectors(Session session)
        {
            List<InspectorData> inspectorsData = session.inspectors;

            foreach (InspectorData idata in inspectorsData)
            {
                Inspector inspector = Instantiator.Instance.Inspector();
                inspector.setContent(idata.rows);
                inspector.transform.position = new Vector3(idata.position.x, idata.position.y, idata.position.z);
                inspector.transform.forward = new Vector3(idata.forward.x, idata.forward.y, idata.forward.z);

                inspectors.Add(inspector);

                InteractionLogger.Count("Inspector", inspector.GetInstanceID().ToString());
            }
        }

        /// <summary>
        /// Stores graphs data
        /// </summary>
        /// <returns>SVGData list</returns>
        public static List<SVGData> SerializeGraphs()
        {
            List<SVGData> graphsData = new List<SVGData>();
            foreach (Graph graph in graphs)
            {
                Vector3 pos = graph.transform.position;
                Vector3 fwd = graph.transform.forward;
                string raw_image = graph.raw_image;
                string type = graph.type;
                graphsData.Add(new SVGData(pos, fwd, raw_image, type));
            }
            return graphsData;
        }

        /// <summary>
        /// Stores graphs data
        /// </summary>
        /// <param name="session">Previous session</param>
        public static void DeserializeGraphs(Session session)
        {
            List<SVGData> graphsData = session.graphs;

            foreach (SVGData gdata in graphsData)
            {
                string rawImage = gdata.rawImage;
                string type = gdata.type;

                Graph graph = Instantiator.Instance.Graph();
                graph.setSprite(rawImage, type);
                graph.transform.position = new Vector3(gdata.position.x, gdata.position.y, gdata.position.z);
                graph.transform.forward = new Vector3(gdata.forward.x, gdata.forward.y, gdata.forward.z);
                graphs.Add(graph);

                InteractionLogger.Count("GraphObject", graph.GetInstanceID().ToString());
            }
        }

        /// <summary>
        /// Saves serialized data into a file.
        /// </summary>
        /// <returns></returns>
        public static void Save()
        {
            if (!Application.isEditor)
            {
                if (!Directory.Exists(Application.persistentDataPath))
                    Directory.CreateDirectory(Application.persistentDataPath);

                Session s = new Session(
                    SerializeBrowsers(),
                    SerializePlaygrounds(),
                    SerializeInspectors(),
                    SerializeGraphs()
                );

                AsynchronousSerializer.Serialize(sessionPath, s);
            }
        }

        /// <summary>
        /// Loads serialized data from a file.
        /// </summary>
        /// <returns></returns>
        public static async Task Load()
        {
            if (!Application.isEditor)
            {
                if (!Directory.Exists(Application.persistentDataPath))
                    Directory.CreateDirectory(Application.persistentDataPath);

                if (File.Exists(sessionPath))
                {
                    Session session = await AsynchronousSerializer.Deserialize(sessionPath);
                    DeserializeBrowsers(session);
                    DeserializePlaygrounds(session);
                    DeserializeInspectors(session);
                    DeserializeGraphs(session);
                }
            }
        }
    }
}