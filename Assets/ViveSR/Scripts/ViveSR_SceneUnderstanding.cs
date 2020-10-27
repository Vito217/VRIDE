using UnityEngine;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Collections;
using System;
using System.IO;

namespace Vive.Plugin.SR
{
    public enum SceneUnderstandingObjectType
    {
        NONE,
        FLOOR,
        WALL,
        CEILING,
        CHAIR,
        TABLE,
        BED,
        MONITOR,
        WINDOW,
        FURNITURE,
        DOOR,
        PICTURE,
        PERSON,
        LIGHT,
        PLANT,
        CURTAIN,
        PILLOW,
        NumOfTypes // always the last
    };

    public class SceneUnderstandingDataReader
    {
        public struct SceneUnderstandingObject
        {
            public string tag;
            public int id;
            public string objfilename;
            public string cldfilename;
            public List<Vector3> position;

            public Vector3 forward;
            public Vector3 center;
            public Vector3 bBoxMinPoint;
            public Vector3 bBoxMaxPoint;
            public List<Vector3> usableLocForNavMesh;
        }
        public SceneUnderstandingDataReader(string fileDir)
        {
            if (Directory.Exists(fileDir))
            {
                DirectoryInfo dir = new DirectoryInfo(fileDir);

                FileInfo[] info_xml = dir.GetFiles("*.xml");
                elements.Clear();
                foreach (FileInfo f in info_xml)
                {
#if UNITY_EDITOR
                    bool res = ViveSR_FileTool.LoadListSerialData(ref elements, f.ToString());
                    if (res == false)
                        Debug.Log("UNITY_EDITOR_NO_FILE:" + f.ToString());
#else
                    string fullPath = dir.FullName + "/" + f;
                    bool res = ViveSR_FileTool.LoadListSerialData(ref elements, fullPath.ToString());
                    if(res==false)
                        Debug.Log("Not UNITY_EDITOR_NO_FILE : " + fullPath.ToString());
#endif

                }
            }
            else
            {
                Debug.Log(fileDir + " folder is empty");
            }
        }
        public void GetElementsBoundingBoxMeshes(int tagObj, SceneUnderstandingObject tagIdElement, ref List<GameObject> boxObj)
        {
            List<int> MeshDataIndices = new List<int>();

            //top lines
            MeshDataIndices.Add(0); MeshDataIndices.Add(1);
            MeshDataIndices.Add(1); MeshDataIndices.Add(2);
            MeshDataIndices.Add(2); MeshDataIndices.Add(3);
            MeshDataIndices.Add(3); MeshDataIndices.Add(0);
            //bottom lines
            MeshDataIndices.Add(4); MeshDataIndices.Add(5);
            MeshDataIndices.Add(5); MeshDataIndices.Add(6);
            MeshDataIndices.Add(6); MeshDataIndices.Add(7);
            MeshDataIndices.Add(7); MeshDataIndices.Add(4);
            //vertical lines
            MeshDataIndices.Add(0); MeshDataIndices.Add(4);
            MeshDataIndices.Add(1); MeshDataIndices.Add(5);
            MeshDataIndices.Add(2); MeshDataIndices.Add(6);
            MeshDataIndices.Add(3); MeshDataIndices.Add(7);


            //foreach (Element each in GetElements(enums[tagObj]))
            {
                GameObject Obj = new GameObject("Box_" + tagIdElement.tag + "_" + tagIdElement.id);
                MeshFilter mf = Obj.AddComponent(typeof(MeshFilter)) as MeshFilter;
                MeshRenderer mr = Obj.AddComponent(typeof(MeshRenderer)) as MeshRenderer;
                mr.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
                mr.material.shader = Shader.Find("UI/Default");
                mr.material.color = objetsColor[tagObj];
                mf.mesh = new Mesh();
                mf.mesh.MarkDynamic();
                List<Vector3> MeshDataVertices = new List<Vector3>();

                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMinPoint.x, tagIdElement.bBoxMinPoint.y, tagIdElement.bBoxMinPoint.z)); //0
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMinPoint.x, tagIdElement.bBoxMinPoint.y, tagIdElement.bBoxMaxPoint.z)); //1
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMaxPoint.x, tagIdElement.bBoxMinPoint.y, tagIdElement.bBoxMaxPoint.z)); //2
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMaxPoint.x, tagIdElement.bBoxMinPoint.y, tagIdElement.bBoxMinPoint.z)); //3
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMinPoint.x, tagIdElement.bBoxMaxPoint.y, tagIdElement.bBoxMinPoint.z)); //4
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMinPoint.x, tagIdElement.bBoxMaxPoint.y, tagIdElement.bBoxMaxPoint.z)); //5
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMaxPoint.x, tagIdElement.bBoxMaxPoint.y, tagIdElement.bBoxMaxPoint.z)); //6
                MeshDataVertices.Add(new Vector3(tagIdElement.bBoxMaxPoint.x, tagIdElement.bBoxMaxPoint.y, tagIdElement.bBoxMinPoint.z)); //7

                mf.sharedMesh.Clear();
                mf.sharedMesh.SetVertices(MeshDataVertices);
                mf.sharedMesh.SetIndices(MeshDataIndices.ToArray(), MeshTopology.Lines, 0);
                Obj.SetActive(false);
                boxObj.Add(Obj);
            }

        }

        public void GetElementsIcons(int tag_obj, SceneUnderstandingObject tagId_element, ref List<GameObject> icon_obj)
        {
            Texture icon_texture = (Texture)Resources.Load(GetElementName(tag_obj));

            Mesh m = new Mesh();
            m.vertices = new Vector3[]
            {
                new Vector3(-0.15f,0,0),
                new Vector3(0.15f,0,0),
                new Vector3(-0.15f,0.3f,0),
                new Vector3(0.15f,0.3f,0)
            };
            m.uv = new Vector2[]
            {
                new Vector2(1,0),
                new Vector2(0,0),
                new Vector2(1,1),
                new Vector2(0,1)
            };
            m.triangles = new int[] { 0, 1, 2, 2, 1, 3 };


            //foreach (Element each in GetElements(enums[tagObj]))
            {
                GameObject Obj = new GameObject("Icon_" + tagId_element.tag + "_" + tagId_element.id);
                MeshFilter mf = Obj.AddComponent(typeof(MeshFilter)) as MeshFilter;
                MeshRenderer mr = Obj.AddComponent(typeof(MeshRenderer)) as MeshRenderer;
                Vector3 iconPosition;
                Vector2 compareX;
                Vector2 compareY;
                Vector2 compareZ;

                if (tagId_element.bBoxMinPoint.x > tagId_element.bBoxMaxPoint.x)
                {
                    compareX = new Vector2(tagId_element.bBoxMinPoint.x, tagId_element.bBoxMaxPoint.x);
                }
                else
                {
                    compareX = new Vector2(tagId_element.bBoxMaxPoint.x, tagId_element.bBoxMinPoint.x);
                }

                if (tagId_element.bBoxMinPoint.y > tagId_element.bBoxMaxPoint.y)
                {
                    compareY = new Vector2(tagId_element.bBoxMinPoint.y, tagId_element.bBoxMaxPoint.y);
                }
                else
                {
                    compareY = new Vector2(tagId_element.bBoxMaxPoint.y, tagId_element.bBoxMinPoint.y);
                }

                if (tagId_element.bBoxMinPoint.z > tagId_element.bBoxMaxPoint.z)
                {
                    compareZ = new Vector2(tagId_element.bBoxMinPoint.z, tagId_element.bBoxMaxPoint.z);
                }
                else
                {
                    compareZ = new Vector2(tagId_element.bBoxMaxPoint.z, tagId_element.bBoxMinPoint.z);
                }

                if (tagId_element.tag == "Chair" || tagId_element.tag == "Table" || tagId_element.tag == "Bed" || tagId_element.tag == "Floor")
                {
                    iconPosition = new Vector3(compareX.y + (compareX.x - compareX.y) / 2, compareY.x, compareZ.y + (compareZ.x - compareZ.y) / 2);
                }
                else if (tagId_element.tag == "Ceiling")
                {
                    iconPosition = new Vector3(compareX.y + (compareX.x - compareX.y) / 2, compareY.y, compareZ.y + (compareZ.x - compareZ.y) / 2);
                }
                else if (tagId_element.tag == "Wall" || tagId_element.tag == "Window" || tagId_element.tag == "Monitor" || tagId_element.tag == "Door" || tagId_element.tag == "Picture")
                {
                    iconPosition = new Vector3(compareX.y + (compareX.x - compareX.y) / 2, compareY.y + (compareY.x - compareY.y) / 2, compareZ.x);
                }
                else 
                {
                    iconPosition = new Vector3(compareX.y + (compareX.x - compareX.y) * 0.5f, compareY.y + (compareY.x - compareY.y) * 0.5f, compareZ.y + (compareZ.x - compareZ.y) * 0.5f);
                }

                Obj.transform.Translate(iconPosition);

                mf.mesh = m;
                m.RecalculateBounds();
                m.RecalculateNormals();
                mr.material.shader = Shader.Find("UI/Default");
                mr.material.mainTexture = icon_texture;
                Obj.SetActive(false);
                icon_obj.Add(Obj);
            }
        }

        public SceneUnderstandingObject[] GetElements(int tag)
        {
            return GetElements(enums[tag]);
        }

        public SceneUnderstandingObject[] GetElements()
        {
            return elements.ToArray();
        }

        public string GetElementName(int tag)
        {
            return enums[tag];
        }

        public int GetNElement()
        {
            return elements.Count;
        }
        
        private Color[] objetsColor = new[] { Color.clear, Color.yellow, Color.blue, Color.cyan, Color.green, Color.red, Color.black, Color.magenta, Color.grey, 
                                            new Color(160.0f, 32.0f, 240.0f), new Color(218.0f, 112.0f, 214.0f), new Color(227.0f, 168.0f, 105.0f), new Color(107.0f, 142.0f, 35.0f),
                                            new Color(80.0f, 50.0f, 50.0f), new Color(255.0f, 255.0f, 120.0f), new Color(189.0f, 252.0f, 201.0f), new Color(120.0f, 60.0f, 60.0f) };
        private String[] enums = new[] { "None", "Floor", "Wall", "Ceiling", "Chair", "Table", "Bed", "Monitor", "Window", "Furniture", "Door", "Picture", "Person", "Light", "Plant", "Curtain","Pillow"};
        private List<SceneUnderstandingObject> elements = new List<SceneUnderstandingObject>();
        private SceneUnderstandingObject[] GetElements(string tag)
        {
            List<SceneUnderstandingObject> matchs = new List<SceneUnderstandingObject>();
            foreach (SceneUnderstandingObject each in elements)
            {
                if (each.tag == tag) matchs.Add(each);
            }
            return matchs.ToArray();
        }
    }

    //The region of Deprecation period API will remove in the future.
    #region Deprecation period API
    /**
    * The class SceneUnderstandingObjects has changed to SceneUnderstandingDataReader.
    * @warning The class will remove in the future.
    */
    public class SceneUnderstandingObjects
    {
        private SceneUnderstandingDataReader new_interface_object;

        public struct Element
        {
            public string tag;
            public int id;
            public string objfilename;
            public string cldfilename;
            public List<Vector3> position;

            public Vector3 forward;
            public Vector3 bBoxMinPoint;
            public Vector3 bBoxMaxPoint;
            public List<Vector3> usableLocForNavMesh;
        }
        private void ConverToNewInterface(Element old_element, ref SceneUnderstandingDataReader.SceneUnderstandingObject new_element)
        {
            new_element.tag = old_element.tag;
            new_element.id = old_element.id;
            new_element.objfilename = old_element.objfilename;
            new_element.cldfilename = old_element.cldfilename;
            new_element.position = old_element.position;
            new_element.forward = old_element.forward;
            new_element.bBoxMinPoint = old_element.bBoxMinPoint;
            new_element.bBoxMaxPoint = old_element.bBoxMaxPoint;
            new_element.usableLocForNavMesh = old_element.usableLocForNavMesh;
        }

        private void ConverToOldInterface(SceneUnderstandingDataReader.SceneUnderstandingObject new_element, ref Element old_element)
        {
            old_element.tag = new_element.tag;
            old_element.id = new_element.id;
            old_element.objfilename = new_element.objfilename;
            old_element.cldfilename = new_element.cldfilename;
            old_element.position = new_element.position;
            old_element.forward = new_element.forward;
            old_element.bBoxMinPoint = new_element.bBoxMinPoint;
            old_element.bBoxMaxPoint = new_element.bBoxMaxPoint;
            old_element.usableLocForNavMesh = new_element.usableLocForNavMesh;
        }

        public SceneUnderstandingObjects(string fileDir)
        {
            new_interface_object = new SceneUnderstandingDataReader(fileDir);
        }

        public void GetElementsBoundingBoxMeshes(int tagObj, Element tagIdElement, ref List<GameObject> boxObj)
        {
            SceneUnderstandingDataReader.SceneUnderstandingObject new_element = new SceneUnderstandingDataReader.SceneUnderstandingObject();
            ConverToNewInterface(tagIdElement, ref new_element);
            new_interface_object.GetElementsBoundingBoxMeshes(tagObj, new_element, ref boxObj);
        }

        public void GetElementsIcons(int tag_obj, Element tagId_element, ref List<GameObject> icon_obj)
        {
            SceneUnderstandingDataReader.SceneUnderstandingObject new_element = new SceneUnderstandingDataReader.SceneUnderstandingObject();
            ConverToNewInterface(tagId_element, ref new_element);
            new_interface_object.GetElementsIcons(tag_obj, new_element, ref icon_obj);
        }

        public Element[] GetElements(int tag)
        {
            List<Element> matchs = new List<Element>();
            SceneUnderstandingDataReader.SceneUnderstandingObject[] new_elements = new_interface_object.GetElements(tag);
            for (int i = 0; i < new_elements.Length; i++)
            {
                Element element = new Element();
                ConverToOldInterface(new_elements[i], ref element);
                matchs.Add(element);
            }
            return matchs.ToArray();
        }

        public Element[] GetElements()
        {
            List<Element> matchs = new List<Element>();
            SceneUnderstandingDataReader.SceneUnderstandingObject[] new_elements = new_interface_object.GetElements();
            for (int i = 0; i < new_elements.Length; i++)
            {
                Element element = new Element();
                ConverToOldInterface(new_elements[i], ref element);
                matchs.Add(element);
            }
            return matchs.ToArray();
        }

        public string GetElementName(int tag)
        {
            return new_interface_object.GetElementName(tag);
        }

        public int GetNElement()
        {
            return new_interface_object.GetNElement();
        }
    }
    #endregion

    [StructLayout(LayoutKind.Sequential)]
    public struct SceneUnderstandingConfig
    {
        public int FloorMaxInst;
        public int WallMaxInst;
        public int CeilingMaxInst;
        public int ChairMaxInst;
        public int TableMaxInst;
        public int BedMaxInst;
        public int MonitorMaxInst;
        public int WindowMaxInst;
        public int FurnitureMaxInst;
        public int DoorMaxInst;
        public int PictureMaxInst;
        public int PersonMaxInst;
        public int LightMaxInst;
        public int PlantMaxInst;
        public int CurtainMaxInst;
        public int PillowMaxInst;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable nFloorMaxInst has changed to FloorMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nFloorMaxInst
        {
            get { return FloorMaxInst; }
            set { FloorMaxInst = value; }
        }
        /**
        * The variable nWallMaxInst has changed to WallMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nWallMaxInst
        {
            get { return WallMaxInst; }
            set { WallMaxInst = value; }
        }
        /**
        * The variable nCeilingMaxInst has changed to CeilingMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nCeilingMaxInst
        {
            get { return CeilingMaxInst; }
            set { CeilingMaxInst = value; }
        }
        /**
        * The variable nChairMaxInst has changed to ChairMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nChairMaxInst
        {
            get { return ChairMaxInst; }
            set { ChairMaxInst = value; }
        }
        /**
        * The variable nTableMaxInst has changed to TableMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nTableMaxInst
        {
            get { return TableMaxInst; }
            set { TableMaxInst = value; }
        }
        /**
        * The variable nBedMaxInst has changed to BedMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nBedMaxInst
        {
            get { return BedMaxInst; }
            set { BedMaxInst = value; }
        }
        /**
        * The variable nMonitorMaxInst has changed to MonitorMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nMonitorMaxInst
        {
            get { return MonitorMaxInst; }
            set { MonitorMaxInst = value; }
        }
        /**
        * The variable nWindowMaxInst has changed to WindowMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nWindowMaxInst
        {
            get { return WindowMaxInst; }
            set { WindowMaxInst = value; }
        }
        /**
        * The variable nFurnitureMaxInst has changed to FurnitureMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nFurnitureMaxInst
        {
            get { return FurnitureMaxInst; }
            set { FurnitureMaxInst = value; }
        }
        /**
        * The variable nDoorMaxInst has changed to DoorMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nDoorMaxInst
        {
            get { return DoorMaxInst; }
            set { DoorMaxInst = value; }
        }
        /**
        * The variable nPictureMaxInst has changed to PictureMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nPictureMaxInst
        {
            get { return PictureMaxInst; }
            set { PictureMaxInst = value; }
        }
        /**
        * The variable nPersonMaxInst has changed to PersonMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nPersonMaxInst
        {
            get { return PersonMaxInst; }
            set { PersonMaxInst = value; }
        }
        /**
        * The variable nLightMaxInst has changed to LightMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nLightMaxInst
        {
            get { return LightMaxInst; }
            set { LightMaxInst = value; }
        }
        /**
        * The variable nPlantMaxInst has changed to PlantMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nPlantMaxInst
        {
            get { return PlantMaxInst; }
            set { PlantMaxInst = value; }
        }
        /**
        * The variable nCurtainMaxInst has changed to CurtainMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nCurtainMaxInst
        {
            get { return CurtainMaxInst; }
            set { CurtainMaxInst = value; }
        }
        /**
        * The variable nPillowMaxInst has changed to PillowMaxInst.
        * @warning The variable will remove in the future.
        */
        public int nPillowMaxInst
        {
            get { return PillowMaxInst; }
            set { PillowMaxInst = value; }
        }
        #endregion
    };

    public class ViveSR_SceneUnderstanding
    {
        public static bool IsEnabledSceneUnderstanding = false;
        public static bool IsEnabledSceneUnderstandingRefinement = true;
        public static bool IsEnabledSceneUnderstandingView = false;
        public static bool IsExportingSceneUnderstandingInfo = false;
        public static string DataDirectory = "SceneUnderstanding/";

        private delegate void ExportProgressCallback(int stage, int percentage);
        private static int ScUndProcessingStage = 0;
        private static int ScUndProcessingProgressBar = 0;

        //The region of Deprecation period API will remove in the future.
        #region Deprecation period API
        /**
        * The variable SemanticObjDir has changed to DataDirectory.
        * @warning The variable will remove in the future.
        */
        public static string SemanticObjDir
        {
            get { return DataDirectory; }
            set { DataDirectory = value; }
        }
        /**
        * The function ExportSceneUnderstandingInfo has changed to StartExporting.
        * @warning The function will remove in the future.
        */
        public static void ExportSceneUnderstandingInfo(string filename)
        {
            StartExporting(filename);
        }
        /**
        * The function SetIconLookAtPlayer has changed to IconLookAt.
        * @warning The function will remove in the future.
        */
        public static void SetIconLookAtPlayer(Transform player)
        {
            IconLookAt(player);
        }
        #endregion

        public static void ResetParameter()
        {
            IsEnabledSceneUnderstanding = false;
            IsEnabledSceneUnderstandingRefinement = true;
            IsEnabledSceneUnderstandingView = false;
            IsExportingSceneUnderstandingInfo = false;
            ScUndProcessingProgressBar = 0;
            ScUndProcessingProgressBar = 0;
        }

        public static void EnableSceneUnderstanding(bool enable, bool enableSemanticFusionAfterScanning)
        {
            int result = SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.SCENE_UNDERSTANDING_ENABLE, enable);
            SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.SEMANTIC_FUSION_ALL_AFTER_SCANNING, enableSemanticFusionAfterScanning);

            if (result == (int)Error.WORK)
                IsEnabledSceneUnderstanding = enable;
            else
            {
                Debug.Log("[ViveSR] [Scene Understanding] Activation/Deactivation failed");
                return;
            }

            if (IsEnabledSceneUnderstanding)
            {
                //result = ViveSR_Framework.RegisterCallback(ViveSR_Framework.MODULE_ID_RIGID_RECONSTRUCTION, (int)ReconstructionCallback.SCENE_UNDERSTANDING_PROGRESS, Marshal.GetFunctionPointerForDelegate((ExportProgressCallback)UpdateSceneUnderstandingProgress));
                if (result != (int)Error.WORK)
                    Debug.Log("[ViveSR] [Scene Understanding] Progress listener failed to register");
            }
            else if (IsEnabledSceneUnderstandingView)
                EnableSceneUnderstandingView(false);
        }

        public static void EnableSceneUnderstandingRefinement(bool enable)
        {
            int result;

            result = SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.SCENE_UNDERSTANDING_REFINEMENT, enable);
            if (result == (int)Error.WORK)
            {
                IsEnabledSceneUnderstandingRefinement = enable;
                //Debug.Log("[ViveSR] [Scene Understanding] Refinement " + (enable ? "enabled" : "disabled"));
            }
            else
                Debug.Log("[ViveSR] [Scene Understanding] Setting Refinement failed");

        }

        public static void EnableSceneUnderstandingView(bool enable)
        {
            int result = 0;

            if (!ViveSR_RigidReconstruction.IsScanning) return;

            result = SRWorkModule_API.SetReconstructionParameterBool((int)ReconstructionParam.SCENE_UNDERSTANDING_MACHINE_VISION, enable);
            if (result == (int)Error.WORK)
            {
                IsEnabledSceneUnderstandingView = enable;
                Debug.Log("[ViveSR] [Scene Understanding] Preview " + (enable ? "enabled" : "disabled"));

                if (IsEnabledSceneUnderstandingView)
                {
                    ViveSR_RigidReconstructionRenderer.EnableSector = false;
                    ViveSR_RigidReconstructionRenderer.SetWireFrameOpaque = false;
                }
                else
                {
                    // ViveSR_RigidReconstructionRenderer.EnableSector = true;
                    ViveSR_RigidReconstructionRenderer.SetWireFrameOpaque = true;

                    ResetSceneUnderstandingProgress();
                }
            }
        }
        private static bool ReconstructionSectorSetting;
        public static void StartExporting(string filename)
        {
            if (ViveSR_DualCameraImageCapture.IsDepthProcessing)
                ViveSR_DualCameraImageCapture.EnableDepthProcess(false);

            ResetSceneUnderstandingProgress();
            //if ((int)Error.WORK != SRWorkModule_API.RegisterSceneUnderstandingCallback(Marshal.GetFunctionPointerForDelegate((ExportProgressCallback)UpdateSceneUnderstandingProgress)))
            //    Debug.Log("[ViveSR] [Scene Understanding] Progress listener failed to register");
            ReconstructionSectorSetting = ViveSR_RigidReconstructionRenderer.EnableSector;
            ViveSR_RigidReconstructionRenderer.EnableSector = false;
            byte[] bytearray = System.Text.Encoding.ASCII.GetBytes(filename);
            IntPtr parameter = Marshal.AllocCoTaskMem(filename.Length);
            Marshal.Copy(bytearray, 0, parameter, filename.Length);
            SRWorkModule_API.SetSceneUnderstandingOutputFileName(parameter, filename.Length);

            IsExportingSceneUnderstandingInfo = true;
        }

        private static void ResetSceneUnderstandingProgress()
        {
            ScUndProcessingProgressBar = 0;
            ScUndProcessingStage = 0;
        }

        private static void UpdateSceneUnderstandingProgress(int stage, int percentage)
        {

            if (stage == (int)ReconstructionExportStage.SCENE_UNDERSTANDING_PASS_1) ScUndProcessingStage = 0;
            else if (stage == (int)ReconstructionExportStage.SCENE_UNDERSTANDING_PASS_2) ScUndProcessingStage = 1;
            ScUndProcessingProgressBar = percentage;
            // Debug.Log("[ViveSR] [Scene Understanding] Progress : " + GetSceneUnderstandingProgress());

            if (IsExportingSceneUnderstandingInfo)
            {
                Debug.Log("[ViveSR] [Scene Understanding] Progress : " + GetSceneUnderstandingProgress());

                if (GetSceneUnderstandingProgress() == 100)
                {
                    Debug.Log("[ViveSR] [Scene Understanding] Finished");
                    //ViveSR_RigidReconstructionRenderer.EnableSector = true;
                    IsExportingSceneUnderstandingInfo = false;
                    //if ((int)Error.WORK != SRWorkModule_API.UnregisterSceneUnderstandingCallback())
                    //    Debug.Log("[ViveSR] [Scene Understanding] Progress listener failed to unregister");
                }
            }
        }

        public static void UpdateSceneUnderstandingProgress()
        {
            SRWorkModule_API.GetSceneUnderstandingProgress(ref ScUndProcessingProgressBar);
            if (ScUndProcessingProgressBar == 100)
            {
                if (!ViveSR_DualCameraImageCapture.IsDepthProcessing)
                    ViveSR_DualCameraImageCapture.EnableDepthProcess(true);

                IsExportingSceneUnderstandingInfo = false;
                ViveSR_RigidReconstructionRenderer.EnableSector = ReconstructionSectorSetting;
            }
        }

        public static void GetSceneUnderstandingProgress(ref int stage, ref int percentage)
        {
            stage = ScUndProcessingStage;
            percentage = ScUndProcessingProgressBar;
        }

        public static int GetSceneUnderstandingProgress()
        {
            return ScUndProcessingProgressBar;
        }

        public static void GetSceneUnderstandingConfig(ref SceneUnderstandingConfig config)
        {
            SRWorkModule_API.GetSceneUnderstandingConfig(ref config);
        }

        public static void SetSceneUnderstandingConfig(SceneUnderstandingConfig config)
        {
            SRWorkModule_API.SetSceneUnderstandingConfig(config);
        }

        public static void SetCustomSceneUnderstandingConfig(SceneUnderstandingObjectType object_type, int objectMaxNum, bool is_on)
        {
            SceneUnderstandingConfig config = new SceneUnderstandingConfig();

            GetSceneUnderstandingConfig(ref config);

            switch (object_type)
            {
                case SceneUnderstandingObjectType.BED:
                    if (is_on) config.BedMaxInst = objectMaxNum;
                    else config.BedMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.CEILING:
                    if (is_on) config.CeilingMaxInst = objectMaxNum;
                    else config.CeilingMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.CHAIR:
                    if (is_on) config.ChairMaxInst = objectMaxNum;
                    else config.ChairMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.FLOOR:
                    if (is_on) config.FloorMaxInst = objectMaxNum;
                    else config.FloorMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.TABLE:
                    if (is_on) config.TableMaxInst = objectMaxNum;
                    else config.TableMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.WALL:
                    if (is_on) config.WallMaxInst = objectMaxNum;
                    else config.WallMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.WINDOW:
                    if (is_on) config.WindowMaxInst = objectMaxNum;
                    else config.WindowMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.MONITOR:
                    if (is_on) config.MonitorMaxInst = objectMaxNum;
                    else config.MonitorMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.FURNITURE:
                    if (is_on) config.FurnitureMaxInst = objectMaxNum;
                    else config.FurnitureMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.DOOR:
                    if (is_on) config.DoorMaxInst = objectMaxNum;
                    else config.DoorMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.PICTURE:
                    if (is_on) config.PictureMaxInst = objectMaxNum;
                    else config.PictureMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.PERSON:
                    if (is_on) config.PersonMaxInst = objectMaxNum;
                    else config.PersonMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.LIGHT:
                    if (is_on) config.LightMaxInst = objectMaxNum;
                    else config.LightMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.PLANT:
                    if (is_on) config.PlantMaxInst = objectMaxNum;
                    else config.PlantMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.CURTAIN:
                    if (is_on) config.CurtainMaxInst = objectMaxNum;
                    else config.CurtainMaxInst = 0;
                    break;
                case SceneUnderstandingObjectType.PILLOW:
                    if (is_on) config.PillowMaxInst = objectMaxNum;
                    else config.PillowMaxInst = 0;
                    break;
            }
            SetSceneUnderstandingConfig(config);
        }

        public static void SetAllCustomSceneUnderstandingConfig(int object_max_num, bool is_on)
        {
            for (int i = 0; i < (int)SceneUnderstandingObjectType.NumOfTypes; i++)
            {
                SetCustomSceneUnderstandingConfig((SceneUnderstandingObjectType)i, object_max_num, is_on);
            }
        }

        public static SceneUnderstandingObjectType GetSemanticTypeFromObjName(string str)
        {
            if (str.Contains("Floor")) return SceneUnderstandingObjectType.FLOOR;
            else if (str.Contains("Wall")) return SceneUnderstandingObjectType.WALL;
            else if (str.Contains("Ceiling")) return SceneUnderstandingObjectType.CEILING;
            else if (str.Contains("Chair")) return SceneUnderstandingObjectType.CHAIR;
            else if (str.Contains("Table")) return SceneUnderstandingObjectType.TABLE;
            else if (str.Contains("Bed")) return SceneUnderstandingObjectType.BED;
            else if (str.Contains("Monitor")) return SceneUnderstandingObjectType.MONITOR;
            else if (str.Contains("Window")) return SceneUnderstandingObjectType.WINDOW;
            else if (str.Contains("Furniture")) return SceneUnderstandingObjectType.FURNITURE;
            else if (str.Contains("Door")) return SceneUnderstandingObjectType.DOOR;
            else if (str.Contains("Picture")) return SceneUnderstandingObjectType.PICTURE;
            else if (str.Contains("Person")) return SceneUnderstandingObjectType.PERSON;
            else if (str.Contains("Light")) return SceneUnderstandingObjectType.LIGHT;
            else if (str.Contains("Plant")) return SceneUnderstandingObjectType.PLANT;
            else if (str.Contains("Curtain")) return SceneUnderstandingObjectType.CURTAIN;
            else if (str.Contains("Pillow")) return SceneUnderstandingObjectType.PILLOW;
            else return SceneUnderstandingObjectType.NONE;
        }

        public static string SemanticTypeToString(SceneUnderstandingObjectType type)
        {
            if (type == SceneUnderstandingObjectType.FLOOR) return "Floor";
            else if (type == SceneUnderstandingObjectType.WALL) return "Wall";
            else if (type == SceneUnderstandingObjectType.CEILING) return "Ceiling";
            else if (type == SceneUnderstandingObjectType.CHAIR) return "Chair";
            else if (type == SceneUnderstandingObjectType.TABLE) return "Table";
            else if (type == SceneUnderstandingObjectType.BED) return "Bed";
            else if (type == SceneUnderstandingObjectType.MONITOR) return "Monitor";
            else if (type == SceneUnderstandingObjectType.WINDOW) return "Window";
            else if (type == SceneUnderstandingObjectType.FURNITURE) return "Furniture";
            else if (type == SceneUnderstandingObjectType.DOOR) return "Door";
            else if (type == SceneUnderstandingObjectType.PICTURE) return "Picture";
            else if (type == SceneUnderstandingObjectType.PERSON) return "Person";
            else if (type == SceneUnderstandingObjectType.LIGHT) return "Light";
            else if (type == SceneUnderstandingObjectType.PLANT) return "Plant";
            else if (type == SceneUnderstandingObjectType.CURTAIN) return "Curtain";
            else if (type == SceneUnderstandingObjectType.PILLOW) return "Pillow";
            else return "NONE";
        }

        public struct SceneObject
        {
            public string Name;
            public SceneUnderstandingObjectType ObjTypeID;
            public int ObjID;
            public string ObjFileName;
            public string CldFileName;
            public Vector3 forward;
            public List<Vector3> positions;
            public List<GameObject> BoundingBoxGameObj;
            public List<GameObject> IconGameObj;
            public Vector3 center;
            public Vector3 bboxMin;
            public Vector3 bboxMax;
            public List<Vector3> usableLocForNavMesh;

            public SceneObject Clone()
            {
                SceneObject output = new SceneObject();
                output = this;
                return output;
            }

            public void SetName(string name)
            {
                this.Name = name;
            }

            public void Clear()
            {
                ObjTypeID = SceneUnderstandingObjectType.NONE;
                ObjID = -1;
                ObjFileName = "";
                CldFileName = "";
                forward = new Vector3();
                center = new Vector3();
                bboxMin = new Vector3();
                bboxMax = new Vector3();

                if (usableLocForNavMesh == null) usableLocForNavMesh = new List<Vector3>();
                else usableLocForNavMesh.Clear();
                if (positions == null) positions = new List<Vector3>();
                else positions.Clear();

                if (BoundingBoxGameObj == null) BoundingBoxGameObj = new List<GameObject>();
                else
                {
                    foreach (GameObject obj in BoundingBoxGameObj)
                    {
                        if (obj != null)
                            GameObject.Destroy(obj);
                    }
                    BoundingBoxGameObj.Clear();
                }

                if (IconGameObj == null) IconGameObj = new List<GameObject>();
                else
                {
                    foreach (GameObject obj in IconGameObj)
                    {
                        if (obj != null)
                            GameObject.Destroy(obj);
                    }
                    IconGameObj.Clear();
                }
            }
        }

        public static List<SceneObject> ShowSceneObjects = new List<SceneObject>();


        public static void DestroySceneObjects()
        {
            foreach (SceneObject obj in ShowSceneObjects) obj.Clear();
            ShowSceneObjects.Clear();
        }

        public static void IconLookAt(Transform player)
        {
            foreach (SceneObject obj in ShowSceneObjects)
            {
                foreach (GameObject icon in obj.IconGameObj)
                {
                    icon.transform.LookAt(player);
                }
            }
        }

        public static bool ShowSemanticBoundingBoxAndIconWithType(SceneUnderstandingObjectType obj_type, bool box_is_visible, bool icon_is_visible)
        {
            bool found = false;
            foreach (SceneObject ssobj in ShowSceneObjects)
            {
                if (ssobj.ObjTypeID == obj_type)
                {
                    found = true;
                    foreach (GameObject obj in ssobj.BoundingBoxGameObj) { if (obj != null) { obj.SetActive(box_is_visible); } }
                    foreach (GameObject obj in ssobj.IconGameObj) { if (obj != null) { obj.SetActive(icon_is_visible); } }
                }
            }
            return found;
        }

        public static void SetAllSemanticBoundingBoxAndIconVisible(bool box_is_visible, bool iconIs_visible)
        {
            foreach (SceneObject ssobj in ShowSceneObjects)
            {
                foreach (GameObject obj in ssobj.BoundingBoxGameObj) { if (obj != null) { obj.SetActive(box_is_visible); } }
                foreach (GameObject obj in ssobj.IconGameObj) { if (obj != null) { obj.SetActive(iconIs_visible); } }
            }
        }

        public static void ShowAllSemanticBoundingBoxAndIcon()
        {
            SetAllSemanticBoundingBoxAndIconVisible(true, true);
        }

        public static void HideAllSemanticBoundingBoxAndIcon()
        {
            SetAllSemanticBoundingBoxAndIconVisible(false, false);
        }

        public static void ShowSemanticBoundingBoxAndIconWithId(SceneUnderstandingObjectType obj_type, int obj_id, bool is_showing_box, bool is_showing_icon)
        {
            foreach (SceneObject ssobj in ShowSceneObjects)
            {
                if (ssobj.ObjTypeID == obj_type && ssobj.ObjID == obj_id)
                {
                    foreach (GameObject obj in ssobj.BoundingBoxGameObj)
                    {
                        if (obj != null)
                            obj.SetActive(is_showing_box);
                    }

                    foreach (GameObject obj in ssobj.IconGameObj)
                    {
                        if (obj != null)
                            obj.SetActive(is_showing_icon);
                    }
                }
            }
        }

        public static void ImportSceneObjectsByType(string dir_path, SceneUnderstandingObjectType obj_type)
        {
            SceneUnderstandingDataReader SceneObj = new SceneUnderstandingDataReader(dir_path);

            foreach (SceneObject ssobj in ShowSceneObjects)
            {
                if (ssobj.ObjTypeID == obj_type)
                {
                    foreach (GameObject obj in ssobj.BoundingBoxGameObj)
                    {
                        if (obj != null)
                            GameObject.Destroy(obj);
                    }
                }
            }
            if (SceneObj.GetNElement() < 1)
            {
                Debug.Log("Scene semantic [" + SceneObj.GetElementName((int)obj_type) + "] data is empty");
                return;
            }

            #region Get Object Bounding Box
            foreach (SceneUnderstandingDataReader.SceneUnderstandingObject element in SceneObj.GetElements((int)obj_type))
            {
                SceneObject scene_obj = new SceneObject();
                scene_obj.BoundingBoxGameObj = new List<GameObject>();
                scene_obj.IconGameObj = new List<GameObject>();
                scene_obj.ObjTypeID = (SceneUnderstandingObjectType)obj_type;
                scene_obj.ObjID = element.id;
                scene_obj.ObjFileName = element.objfilename;
                scene_obj.CldFileName = element.cldfilename;
                scene_obj.forward = element.forward;
                scene_obj.center = element.center;
                scene_obj.positions = element.position;
                scene_obj.usableLocForNavMesh = element.usableLocForNavMesh;
                SceneObj.GetElementsBoundingBoxMeshes((int)obj_type, element, ref scene_obj.BoundingBoxGameObj);
                SceneObj.GetElementsIcons((int)obj_type, element, ref scene_obj.IconGameObj);
                ShowSceneObjects.Add(scene_obj);
            }
            #endregion
        }

        public static void ImportSceneObjects(string dir_path)
        {
            // clear
            foreach (SceneObject obj in ShowSceneObjects) obj.Clear();

            ShowSceneObjects.Clear();

            SceneUnderstandingDataReader SceneObj = new SceneUnderstandingDataReader(dir_path);
            if (SceneObj.GetNElement() < 1)
            {
                Debug.Log("Scene object data in " + dir_path + " is empty.");
                return;
            }

            #region Set scene object data
            for (int objType = 0; objType < (int)SceneUnderstandingObjectType.NumOfTypes; objType++)
            {
                foreach (SceneUnderstandingDataReader.SceneUnderstandingObject element in SceneObj.GetElements(objType))
                {
                    SceneObject scene_obj = new SceneObject();
                    scene_obj.BoundingBoxGameObj = new List<GameObject>();
                    scene_obj.IconGameObj = new List<GameObject>();
                    scene_obj.ObjTypeID = (SceneUnderstandingObjectType)objType;
                    scene_obj.ObjID = element.id;
                    scene_obj.ObjFileName = element.objfilename;
                    scene_obj.CldFileName = element.cldfilename;
                    scene_obj.forward = element.forward;
                    scene_obj.center = element.center;
                    scene_obj.positions = element.position;
                    scene_obj.bboxMin = element.bBoxMinPoint;
                    scene_obj.bboxMax = element.bBoxMaxPoint;
                    scene_obj.usableLocForNavMesh = element.usableLocForNavMesh;
                    SceneObj.GetElementsBoundingBoxMeshes(objType, element, ref scene_obj.BoundingBoxGameObj);
                    SceneObj.GetElementsIcons(objType, element, ref scene_obj.IconGameObj);
                    ShowSceneObjects.Add(scene_obj);
                }
            }
            #endregion
        }

        public static void SetGameObjectByFileName(string fileName, string go_name)
        {
            for (int i = 0; i < ShowSceneObjects.Count; i++)
                if (ShowSceneObjects[i].CldFileName == fileName || ShowSceneObjects[i].ObjFileName == fileName)
                {
                    SceneObject obj = ShowSceneObjects[i].Clone();
                    obj.Name = go_name;
                    ShowSceneObjects[i] = obj;
                }
        }

        public static SceneObject GetCorrespondingSceneObject(string go_name)
        {
            foreach (SceneObject obj in ShowSceneObjects)
            {
                if (obj.Name == go_name)
                    return obj.Clone();
            }

            SceneObject emptyObj = new SceneObject();
            emptyObj.Clear();
            return emptyObj;
        }

        public static string[] GetColliderFileNames()
        {
            List<string> nameList = new List<string>();
            foreach (SceneObject obj in ShowSceneObjects) nameList.Add(obj.CldFileName);
            return nameList.ToArray();
        }

        public static string[] GetColliderFileNamesByType(SceneUnderstandingObjectType obj_type)
        {
            List<string> nameList = new List<string>();
            foreach (SceneObject obj in ShowSceneObjects)
                if (obj_type == obj.ObjTypeID)
                    nameList.Add(obj.CldFileName);

            return nameList.ToArray();
        }

        public static List<Vector3> GetPlacedPositionsByID(SceneUnderstandingObjectType obj_type, int obj_id)
        {
            List<Vector3> positions = new List<Vector3>();
            foreach (SceneObject obj in ShowSceneObjects)
            {
                if (obj.ObjTypeID == obj_type && obj.ObjID == obj_id)
                    positions.AddRange(obj.positions);
            }
            return positions;
        }

        public static List<Vector3> GetPlacedPositionsByType(SceneUnderstandingObjectType obj_type)
        {
            List<Vector3> positions = new List<Vector3>();
            foreach (SceneObject obj in ShowSceneObjects)
            {
                if (obj.ObjTypeID == obj_type)
                    positions.AddRange(obj.positions);
            }
            return positions;
        }

        public static List<Vector3> GetAllPlacedPositions()
        {
            List<Vector3> positions = new List<Vector3>();
            foreach (SceneObject obj in ShowSceneObjects)
            {
                positions.AddRange(obj.positions);
            }
            return positions;
        }
    };

}
