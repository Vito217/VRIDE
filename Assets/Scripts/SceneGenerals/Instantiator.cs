using UnityEngine;

public class Instantiator: MonoBehaviour
{
    public BrowserClass browserClassPrefab;
    public BrowserMethod browserMethodPrefab;
    public BrowserPackage browserPackagePrefab;
    public RoassalClass roassalClassPrefab;
    public RoassalMethod roassalMethodPrefab;
    public RoassalExamples roassalExamplesPrefab;
    public InspectorRow inspectorRowPrefab;
    public Browser browserPrefab;
    public Playground playgroundPrefab;
    public Inspector inspectorPrefab;
    public Graph svgPrefab;
    public Transcript transcriptPrefab;
    public VRIDEMenu menuPrefab;

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

    public BrowserClass ClassObject(string className, Browser browser)
    {
        BrowserClass new_class = Instantiate(
            browserClassPrefab, browser.class_list.transform, false);
        new_class.name = className;
        new_class.theBrowser = browser;
        return new_class;
    }

    public BrowserPackage PackageObject(string packageName, Browser browser)
    {
        BrowserPackage newPackage = Instantiate(
            browserPackagePrefab, browser.package_list.transform, false);
        newPackage.name = packageName;
        newPackage.theBrowser = browser;
        return newPackage;
    }

    public BrowserMethod MethodObject(string methodName, Browser browser)
    {
        BrowserMethod new_method = Instantiate(
            browserMethodPrefab, browser.methodList.transform, false);
        new_method.name = methodName;
        new_method.theBrowser = browser;
        return new_method;
    }

    public RoassalClass RoassalClassObject(string className, RoassalExamples roassal)
    {
        RoassalClass new_class = Instantiate(
            roassalClassPrefab, roassal.class_list.transform, false);
        new_class.name = className;
        new_class.roassal = roassal;
        return new_class;
    }

    public RoassalMethod RoassalMethodObject(string methodName, RoassalExamples roassal)
    {
        RoassalMethod new_method = Instantiate(
            roassalMethodPrefab, roassal.methodList.transform, false);
        new_method.name = methodName;
        new_method.roassal = roassal;
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

    public VRIDEMenu Menu()
    {
        return Instantiate(menuPrefab);
    }

    public RoassalExamples RoassalExamples()
    {
        return Instantiate(roassalExamplesPrefab);
    }
}
