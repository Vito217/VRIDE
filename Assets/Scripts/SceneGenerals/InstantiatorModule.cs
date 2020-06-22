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
        public static string classPath = "Classes/Panel/Scroll View/Viewport/Content";
        public static string methodPath = "Methods/Panel/Scroll View/Viewport/Content";
        public static string inspectedPath = "InspectorTable/Panel/Scroll View/Viewport/Content";

        public static BrowserClass browserClassPrefab = Resources.Load<BrowserClass>(oldPrefabs + "BrowserClass");
        public static BrowserMethod browserMethodPrefab = Resources.Load<BrowserMethod>(oldPrefabs + "BrowserMethod");
        public static Transform classMethodListPrefab = Resources.Load<Transform>(oldPrefabs + "ClassMethodList");
        public static InspectorRow inspectorRowPrefab = Resources.Load<InspectorRow>(oldPrefabs + "InspectorRow");
        public static InitializeBehaviour browserPrefab = Resources.Load<BrowserInit>(prefabs + "Browser");
        public static InitializeBehaviour playgroundPrefab = Resources.Load<PlaygroundInit>(prefabs + "Playground");
        public static InitializeBehaviour inspectorPrefab = Resources.Load<InspectorInit>(prefabs + "Inspector");
        public static InitializeBehaviour svgPrefab = Resources.Load<SVGObjectInit>(prefabs + "GraphObject");

        public static BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
            Transform methodList)
        {
            BrowserClass new_class = UnityEngine.Object.Instantiate(browserClassPrefab, parentWindow.transform, false);
            new_class.gameObject.GetComponent<TextMeshProUGUI>().text = className;
            new_class.gameObject.name = className;
            new_class.name = className;
            new_class.field = field;
            new_class.parent_window = parentWindow;
            new_class.method_list = methodList.gameObject;
            return new_class;
        }

        public static BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
            Transform methodList, string sourceCode)
        {
            BrowserClass new_class = ClassObject(parentWindow, className, field, methodList);
            new_class.sourceCode = sourceCode;
            return new_class;
        }

        public static Transform MethodListObject(Transform methodListContent, string className, TMP_InputField field)
        {
            Transform new_method_list = UnityEngine.Object.Instantiate(classMethodListPrefab, methodListContent, false);
            new_method_list.Find("template").gameObject.GetComponent<BrowserMethod>().field = field;
            new_method_list.name = className;
            new_method_list.gameObject.SetActive(false);
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

        public static InitializeBehaviour Browser(SystemData data)
        {
            InitializeBehaviour browser = UnityEngine.Object.Instantiate(browserPrefab);

            ClassWindow classList = browser.transform.Find(classPath).gameObject.GetComponent<ClassWindow>();
            TMP_InputField field = browser.transform.Find(editorPath).gameObject.GetComponent<TMP_InputField>();
            Transform methodList = browser.transform.Find(methodPath);

            bool first = true;
            foreach (Tuple<string, string> classAndCode in data.classes)
            {
                string className = classAndCode.Item1;
                string classCode = classAndCode.Item2;

                Transform classMethodList = MethodListObject(methodList, className, field);
                BrowserClass c = ClassObject(classList, className, field, classMethodList, classCode);

                List<Tuple<string, string>> methods = data.methodLists[className];

                foreach (Tuple<string, string> methodAndCode in methods)
                {
                    string methodName = methodAndCode.Item1;
                    string methodCode = methodAndCode.Item2;

                    BrowserMethod m = MethodObject(classMethodList, className, methodName, field, methodCode);
                }

                if (first) { c.click(); first = false; }
            }

            return browser;
        }

        public static InitializeBehaviour Inspector()
        {
            return UnityEngine.Object.Instantiate(inspectorPrefab);
        }

        public static InitializeBehaviour Graph()
        {
            return UnityEngine.Object.Instantiate(svgPrefab);
        }
    }
}
