using UnityEngine;
using TMPro;

public class Instantiator: MonoBehaviour
{
    public BrowserClass browserClassPrefab;
    public BrowserMethod browserMethodPrefab;
    public BrowserPackage browserPackagePrefab;
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

    public BrowserClass ClassObject(string className, Browser browser)
    {
        BrowserClass new_class = Instantiate(
            browserClassPrefab, browser.class_list.transform, false);
        new_class.gameObject.GetComponent<TextMeshProUGUI>().text = className;
        new_class.gameObject.name = className;
        new_class.theBrowser = browser;
        return new_class;
    }

    public BrowserPackage PackageObject(string packageName, Browser browser)
    {
        BrowserPackage newPackage = Instantiate(
            browserPackagePrefab, browser.package_list.transform, false);
        newPackage.gameObject.GetComponent<TextMeshProUGUI>().text = packageName;
        newPackage.gameObject.name = packageName;
        newPackage.theBrowser = browser;
        return newPackage;
    }

    public BrowserMethod MethodObject(string methodName, Browser browser)
    {
        BrowserMethod new_method = Instantiate(
            browserMethodPrefab, browser.methodList.transform, false);
        new_method.gameObject.GetComponent<TextMeshProUGUI>().text = methodName;
        new_method.gameObject.name = methodName;
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
