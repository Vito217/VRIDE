using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

namespace InstantiatorModule
{
    public static class Instantiator
    {
        public static string oldPrefabs = "Prefabs/2.0/";
        public static string prefabs = "Prefabs/3.0/UI/";
        public static string editorPath = "Editor/Panel/InputField (TMP)";
        public static string packagePath = "Objects/Packages/Panel/Scroll View/Viewport/Content";
        public static string classPath = "Objects/Classes/Panel/Scroll View/Viewport/Content";
        public static string methodPath = "Objects/Methods/Panel/Scroll View/Viewport/Content";
        public static string inspectedPath = "InspectorTable/Panel/Scroll View/Viewport/Content";

        public static BrowserClass browserClassPrefab = Resources.Load<BrowserClass>(oldPrefabs + "BrowserClass");
        public static BrowserMethod browserMethodPrefab = Resources.Load<BrowserMethod>(oldPrefabs + "BrowserMethod");
        public static BrowserPackage browserPackagePrefab = Resources.Load<BrowserPackage>(oldPrefabs + "BrowserPackage");
        public static Transform classMethodListPrefab = Resources.Load<Transform>(oldPrefabs + "ClassMethodList");
        public static ClassWindow packageClassListPrefab = Resources.Load<ClassWindow>(oldPrefabs + "PackageClassList");
        public static InspectorRow inspectorRowPrefab = Resources.Load<InspectorRow>(oldPrefabs + "InspectorRow");
        public static Browser browserPrefab = Resources.Load<Browser>(prefabs + "Browser");
        public static InitializeBehaviour playgroundPrefab = Resources.Load<Playground>(prefabs + "Playground");
        public static InitializeBehaviour inspectorPrefab = Resources.Load<Inspector>(prefabs + "Inspector");
        public static InitializeBehaviour svgPrefab = Resources.Load<Graph>(prefabs + "GraphObject");
        public static InitializeBehaviour transcriptPrefab = Resources.Load<Transcript>(prefabs + "Transcript");

        public static BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
            Transform classSideMethodList, Transform instanceSideMethodList)
        {
            BrowserClass new_class = UnityEngine.Object.Instantiate(browserClassPrefab, parentWindow.transform, false);
            new_class.gameObject.GetComponent<TextMeshProUGUI>().text = className;
            new_class.gameObject.name = className;
            new_class.name = className;
            new_class.field = field;
            new_class.parent_window = parentWindow;
            new_class.classMethodList = classSideMethodList;
            new_class.instanceMethodList = instanceSideMethodList;
            return new_class;
        }

        public static BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
            Transform classSideMethodList, Transform instanceSideMethodList, string sourceCode, Browser browser)
        {
            BrowserClass new_class = ClassObject(parentWindow, className, field, classSideMethodList, instanceSideMethodList);
            new_class.sourceCode = sourceCode;
            new_class.theBrowser = browser;
            return new_class;
        }

        public static BrowserPackage PackageObject(PackageWindow parentWindow, string packageName, TMP_InputField field,
            ClassWindow classList, Browser browser)
        {
            BrowserPackage newPackage = UnityEngine.Object.Instantiate(browserPackagePrefab, parentWindow.transform, false);
            newPackage.gameObject.GetComponent<TextMeshProUGUI>().text = packageName;
            newPackage.gameObject.name = packageName;
            newPackage.name = packageName;
            newPackage.field = field;
            newPackage.parentWindow = parentWindow;
            newPackage.classList = classList;
            newPackage.theBrowser = browser;
            return newPackage;
        }

        public static ClassWindow ClassListObject(Transform classListContent, string packageName, TMP_InputField field)
        {
            ClassWindow newClassList = UnityEngine.Object.Instantiate(packageClassListPrefab, classListContent, false);
            newClassList.transform.Find("template").gameObject.GetComponent<BrowserClass>().field = field;
            newClassList.name = packageName;
            return newClassList;
        }

        public static Transform MethodListObject(Transform methodListContent, string className, TMP_InputField field)
        {
            Transform new_method_list = UnityEngine.Object.Instantiate(classMethodListPrefab, methodListContent, false);
            new_method_list.Find("template").gameObject.GetComponent<BrowserMethod>().field = field;
            new_method_list.name = className;
            return new_method_list;
        }

        public static BrowserMethod MethodObject(Transform parentWindow, string className, string methodName, 
            TMP_InputField field)
        {
            BrowserMethod new_method = UnityEngine.Object.Instantiate(browserMethodPrefab, parentWindow, false);
            new_method.gameObject.GetComponent<TextMeshProUGUI>().text = methodName;
            new_method.gameObject.name = methodName;
            new_method.field = field;
            return new_method;
        }

        public static BrowserMethod MethodObject(Transform parentWindow, string className, string methodName,
            TMP_InputField field, string sourceCode)
        {
            BrowserMethod new_method = MethodObject(parentWindow, className, methodName, field);
            new_method.sourceCode = sourceCode;
            return new_method;
        }

        public static InitializeBehaviour Playground()
        {
            return UnityEngine.Object.Instantiate(playgroundPrefab);
        }

        public static Browser Browser()
        {
            return UnityEngine.Object.Instantiate(browserPrefab);
        }

        public static InitializeBehaviour Inspector()
        {
            return UnityEngine.Object.Instantiate(inspectorPrefab);
        }

        public static InitializeBehaviour Graph()
        {
            return UnityEngine.Object.Instantiate(svgPrefab);
        }

        public static InitializeBehaviour Transcript()
        {
            InitializeBehaviour t = UnityEngine.Object.Instantiate(transcriptPrefab);
            t.gameObject.GetComponent<Transcript>().field.text = VRIDEController.transcriptContents;
            return t;
        }

        public static InspectorRow InspectorDataRow()
        {
            return UnityEngine.Object.Instantiate(inspectorRowPrefab);
        }
    }
}
