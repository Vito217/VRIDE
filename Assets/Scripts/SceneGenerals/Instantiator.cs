using UnityEngine;
using TMPro;

public class Instantiator: MonoBehaviour
{
    public BrowserClass browserClassPrefab;
    public BrowserMethod browserMethodPrefab;
    public BrowserPackage browserPackagePrefab;
    public MethodWindow classMethodListPrefab;
    public ClassWindow packageClassListPrefab;
    public InspectorRow inspectorRowPrefab;
    public Browser browserPrefab;
    public Playground playgroundPrefab;
    public Inspector inspectorPrefab;
    public Graph svgPrefab;
    public Transcript transcriptPrefab;

    private static Instantiator _instance;
    public static Instantiator Instance
    {
        get { return _instance; }
    }

    private void Awake()
    {
        if (_instance != null && _instance != this)
            Destroy(this.gameObject);
        else
            _instance = this;
    }

    public BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
        MethodWindow classSideMethodList, MethodWindow instanceSideMethodList)
    {
        BrowserClass new_class = Instantiate(browserClassPrefab, parentWindow.transform, false);
        new_class.gameObject.GetComponent<TextMeshProUGUI>().text = className;
        new_class.gameObject.name = className;
        new_class.name = className;
        //new_class.parent_window = parentWindow;
        //new_class.classMethodList = classSideMethodList;
        //new_class.instanceMethodList = instanceSideMethodList;
        return new_class;
    }

    public BrowserClass ClassObject(ClassWindow parentWindow, string className, TMP_InputField field,
        MethodWindow classSideMethodList, MethodWindow instanceSideMethodList, string sourceCode, Browser browser)
    {
        BrowserClass new_class = ClassObject(parentWindow, className, field, classSideMethodList, instanceSideMethodList);
        new_class.sourceCode = sourceCode;
        new_class.theBrowser = browser;
        return new_class;
    }

    public BrowserPackage PackageObject(PackageWindow parentWindow, string packageName, 
        ClassWindow classList, Browser browser)
    {
        BrowserPackage newPackage = Instantiate(browserPackagePrefab, parentWindow.transform, false);
        newPackage.gameObject.GetComponent<TextMeshProUGUI>().text = packageName;
        newPackage.gameObject.name = packageName;
        newPackage.name = packageName;
        //newPackage.parentWindow = parentWindow;
        //newPackage.classList = classList;
        newPackage.theBrowser = browser;
        return newPackage;
    }

    public ClassWindow ClassListObject(Transform classListContent, string packageName, Browser browser)
    {
        ClassWindow newClassList = Instantiate(packageClassListPrefab, classListContent, false);
        newClassList.transform.Find("template").gameObject.GetComponent<BrowserClass>().theBrowser = browser;
        newClassList.theBrowser = browser;
        newClassList.name = packageName;
        return newClassList;
    }

    public MethodWindow MethodListObject(Transform methodListContent, string className, Browser browser, 
        string side, string package)
    {
        MethodWindow new_method_list = Instantiate(classMethodListPrefab, methodListContent, false);
        new_method_list.transform.Find("template").gameObject.GetComponent<BrowserMethod>().theBrowser = browser;
        new_method_list.theBrowser = browser;
        new_method_list.name = className;
        new_method_list.side = side;
        new_method_list.package = package;
        return new_method_list;
    }

    public BrowserMethod MethodObject(Transform parentWindow, string className, string methodName, 
        TMP_InputField field)
    {
        BrowserMethod new_method = Instantiate(browserMethodPrefab, parentWindow, false);
        new_method.gameObject.GetComponent<TextMeshProUGUI>().text = methodName;
        new_method.gameObject.name = methodName;
        return new_method;
    }

    public BrowserMethod MethodObject(Transform parentWindow, string className, string methodName,
        TMP_InputField field, string sourceCode, Browser browser)
    {
        BrowserMethod new_method = MethodObject(parentWindow, className, methodName, field);
        new_method.sourceCode = sourceCode;
        new_method.theBrowser = browser;
        return new_method;
    }

    public Playground Playground()
    {
        return Instantiate(playgroundPrefab);
    }

    public Browser Browser()
    {
        return Instantiate(browserPrefab);
    }

    public Inspector Inspector()
    {
        return Instantiate(inspectorPrefab);
    }

    public Graph Graph()
    {
        return Instantiate(svgPrefab);
    }

    public InspectorRow InspectorDataRow()
    {
        return Instantiate(inspectorRowPrefab);
    }

    public Transcript Transcript()
    {
        return Instantiate(transcriptPrefab);
    }
}
