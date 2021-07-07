using UnityEngine;
using TMPro;

/// <summary>
/// Instantiates many prefabs assigned by the user. 
/// </summary>
public class Instantiator : MonoBehaviour
{
    public BrowserClass browserClassPrefab;
    public BrowserMethod browserMethodPrefab;
    public BrowserPackage browserPackagePrefab;
    public BrowserSender browserSenderPrefab;
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
    public WebcamView webcamPrefab;
    public Board boardPrefab;
    public GameObject aframePrefab;
    public TextMeshPro textPrefab;
    public FileExplorer fileExplorerPrefab;
    public PythonEditor pythonEditorPrefab;
    public REditor rEditorPrefab;
    public DesktopView desktopViewPrefab;
    public DesktopWindowObject desktopWindowObjectPrefab;
    public DesktopWindowsExplorer desktopWindowsExplorerPrefab;
    public Keyboards virtualKeyBoardPrefab;
    public Keyboards virtualKeyBoard2Prefab;
    public BrowserWindowCube browserWindowCubePrefab;
    public PlaygroundWindowCube playgroundWindowCubePrefab;
    public ClassWindowCube classWindowCubePrefab;

    public GameObject defaultGround;
    public GameObject spaceShip;
    public GameObject forest;

    public GameObject explorerFilePrefab;
    public GameObject explorerDirectoryPrefab;

    public Material lineRendererMaterial;

    public static GameObject currentEnvironment;

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

        currentEnvironment = GameObject.Find("Ground");
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
    /// Creates a file explorer
    /// </summary>
    /// <returns></returns>
    public FileExplorer FileExplorer()
    {
        return Instantiate(fileExplorerPrefab);
    }

    /// <summary>
    /// Creates a TaskList
    /// </summary>
    /// <returns>TaskList gameObject</returns>
    public WebcamView WebCam()
    {
        return Instantiate(webcamPrefab);
    }

    /// <summary>
    /// Creates a board
    /// </summary>
    /// <returns></returns>
    public Board Board()
    {
        return Instantiate(boardPrefab);
    }

    public PythonEditor PythonEditor()
    {
        return Instantiate(pythonEditorPrefab);
    }

    public REditor REditor()
    {
        return Instantiate(rEditorPrefab);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="content"></param>
    /// <param name="parent"></param>
    /// <returns></returns>
    public TextMeshPro Text(string content, Transform parent)
    {
        TextMeshPro t = Instantiate(textPrefab, parent);
        t.text = content;
        return t;
    }

    /// <summary>
    /// Creates a file as a child of a explorer
    /// </summary>
    /// <param name="name"></param>
    /// <param name="parent"></param>
    /// <returns></returns>
    public GameObject ExplorerFile(string name, Transform parent)
    {
        GameObject file = Instantiate(explorerFilePrefab, parent, false);
        file.GetComponent<TextMeshProUGUI>().text = name;
        return file;
    }

    /// <summary>
    /// Creates a dir as a child of a explorer
    /// </summary>
    /// <param name="name"></param>
    /// <param name="parent"></param>
    /// <returns></returns>
    public GameObject ExplorerDirectory(string name, Transform parent)
    {
        GameObject dir = Instantiate(explorerDirectoryPrefab, parent, false);
        dir.GetComponent<TextMeshProUGUI>().text = name;
        return dir;
    }

    /// <summary>
    /// Creates a streaming remote window
    /// </summary>
    /// <param name="hwnd"></param>
    /// <param name="windowName"></param>
    /// <returns></returns>
    public DesktopView DesktopView(string hwnd, string windowName)
    {
        DesktopView dv = Instantiate(desktopViewPrefab);
        dv.transform.Find("Panel/Toolbar/Button/Text (TMP)").GetComponent<TextMeshProUGUI>().text = windowName;
        dv.name = "ExtWindow:" + hwnd;
        dv.key = hwnd;

        return dv;
    }

    /// <summary>
    /// Creates a list of available streaming windows
    /// </summary>
    /// <returns></returns>
    public DesktopWindowsExplorer DesktopWindowsExplorer()
    {
        return Instantiate(desktopWindowsExplorerPrefab);
    }

    /// <summary>
    /// Creates a virtual keyboard
    /// </summary>
    /// <returns></returns>
    public Keyboards VirtualKeyboard()
    {
        return Instantiate(virtualKeyBoardPrefab);
    }

    public Keyboards VirtualKeyboard2()
    {
        return Instantiate(virtualKeyBoard2Prefab);
    }

    /// <summary>
    /// Creates a child of streaming windows list
    /// </summary>
    /// <param name="hwnd"></param>
    /// <param name="windowName"></param>
    /// <param name="explorer"></param>
    /// <returns></returns>
    public DesktopWindowObject DesktopWindowObject(string hwnd, string windowName, DesktopWindowsExplorer explorer)
    {
        DesktopWindowObject ob = Instantiate(desktopWindowObjectPrefab, explorer.contentList);
        ob.GetComponent<TextMeshProUGUI>().text = windowName;
        ob.windowName = windowName;
        ob.explorer = explorer;
        ob.hwnd = hwnd;

        return ob;
    }

    public BrowserWindowCube BrowserWindowCube()
    {
        return Instantiate(browserWindowCubePrefab);
    }

    public PlaygroundWindowCube PlaygroundWindowCube()
    {
        return Instantiate(playgroundWindowCubePrefab);
    }

    public BrowserSender SenderObject(string senderName, Browser browser)
    {
        BrowserSender newSender = Instantiate(
            browserSenderPrefab, browser.senderList.transform, false);
        newSender.name = senderName;
        newSender.theBrowser = browser;
        return newSender;
    }

    public ClassWindowCube ClassWindowCube()
    {
        return Instantiate(classWindowCubePrefab);
    }
}
