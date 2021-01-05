using UnityEngine;
using TMPro;

/// <summary>
/// Instantiates many prefabs assigned by the user. 
/// </summary>
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
    public GameObject taskListPrefab;
    public GameObject aframePrefab;
    public TextMeshPro textPrefab;

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

    /// <summary>
    /// Creates a Browser Class
    /// </summary>
    /// <param name="className">Name of the class</param>
    /// <param name="browser">Target Browser</param>
    /// <returns>A BrowserClass object</returns>
    public BrowserClass ClassObject(string className, Browser browser)
    {
        BrowserClass new_class = Instantiate(
            browserClassPrefab, browser.class_list.transform, false);
        new_class.name = className;
        new_class.theBrowser = browser;
        return new_class;
    }

    /// <summary>
    /// Creates a Browser Package
    /// </summary>
    /// <param name="packageName">Name of the package</param>
    /// <param name="browser">Target Browser</param>
    /// <returns>A BrowserPackage object</returns>
    public BrowserPackage PackageObject(string packageName, Browser browser)
    {
        BrowserPackage newPackage = Instantiate(
            browserPackagePrefab, browser.package_list.transform, false);
        newPackage.name = packageName;
        newPackage.theBrowser = browser;
        return newPackage;
    }

    /// <summary>
    /// Creates a Browser Method
    /// </summary>
    /// <param name="methodName">Name of the method</param>
    /// <param name="browser">Target browser</param>
    /// <returns>a BrowserMethod object</returns>
    public BrowserMethod MethodObject(string methodName, Browser browser)
    {
        BrowserMethod new_method = Instantiate(
            browserMethodPrefab, browser.methodList.transform, false);
        new_method.name = methodName;
        new_method.theBrowser = browser;
        return new_method;
    }

    /// <summary>
    /// Creates a RoassalClass object
    /// </summary>
    /// <param name="className">Name of the class</param>
    /// <param name="roassal">Target Roassal Examples window</param>
    /// <returns>A RoassalClass object</returns>
    public RoassalClass RoassalClassObject(string className, RoassalExamples roassal)
    {
        RoassalClass new_class = Instantiate(
            roassalClassPrefab, roassal.class_list.transform, false);
        new_class.name = className;
        new_class.roassal = roassal;
        return new_class;
    }

    /// <summary>
    /// Creates a RoassalClass object
    /// </summary>
    /// <param name="className">Name of the class</param>
    /// <param name="roassal">Target Roassal Examples window</param>
    /// <returns>A RoassalClass object</returns>
    public RoassalMethod RoassalMethodObject(string methodName, RoassalExamples roassal)
    {
        RoassalMethod new_method = Instantiate(
            roassalMethodPrefab, roassal.methodList.transform, false);
        new_method.name = methodName;
        new_method.roassal = roassal;
        return new_method;
    }

    /// <summary>
    /// Creates a Playground
    /// </summary>
    /// <returns>Playground object</returns>
    public Playground Playground()
    {
        return Instantiate(playgroundPrefab);
    }

    /// <summary>
    /// Creates a Browser
    /// </summary>
    /// <returns>Browser object</returns>
    public Browser Browser()
    {
        return Instantiate(browserPrefab);
    }

    /// <summary>
    /// Creates an Inspector
    /// </summary>
    /// <returns>Inspector object</returns>
    public Inspector Inspector()
    {
        return Instantiate(inspectorPrefab);
    }
    /// <summary>
    /// Creates a Graph
    /// </summary>
    /// <returns>Graph object</returns>
    public Graph Graph()
    {
        return Instantiate(svgPrefab);
    }

    /// <summary>
    /// Creates a row for the Inspector
    /// </summary>
    /// <returns>InspectorRow object</returns>
    public InspectorRow InspectorDataRow()
    {
        return Instantiate(inspectorRowPrefab);
    }

    /// <summary>
    /// Creates a Transcript
    /// </summary>
    /// <returns>Transcript object</returns>
    public Transcript Transcript()
    {
        return Instantiate(transcriptPrefab);
    }

    /// <summary>
    /// Creates a Menu
    /// </summary>
    /// <returns>VRIDEMenu object</returns>
    public VRIDEMenu Menu()
    {
        return Instantiate(menuPrefab);
    }

    /// <summary>
    /// Creates a Roassal Examples window
    /// </summary>
    /// <returns>RoassalExamples object</returns>
    public RoassalExamples RoassalExamples()
    {
        return Instantiate(roassalExamplesPrefab);
    }

    /// <summary>
    /// Creates an AFrame
    /// </summary>
    /// <returns>AFrame gameObject</returns>
    public GameObject AFrame()
    {
        return Instantiate(aframePrefab);
    }

    /// <summary>
    /// Creates a TaskList
    /// </summary>
    /// <returns>TaskList gameObject</returns>
    public GameObject TaskList()
    {
        return Instantiate(taskListPrefab);
    }

    public TextMeshPro Text(string content, Transform parent)
    {
        TextMeshPro t = Instantiate(textPrefab, parent);
        t.text = content;
        return t;
    }
}
