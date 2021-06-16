using PharoModule;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TMPro;
using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class PharoPackageCodeCube : PharoCodeCube
{
    [HideInInspector]
    public string packageName;
    [HideInInspector]
    public string[] classes;

    public CodeCubeText codeCubeTextPrefab;
    public PharoClassCodeCube classCodeCubePrefab;
    public AFrameLine aFrameLinePrefab;

    private bool opened = false;
    private bool loading = false;

    public override void InnerStart()
    {
        base.InnerStart();
    }

    public void OnActivate()
    {
        if (!loading)
        {
            if (!opened)
            {
                Open();
            }
            else
            {
                Close();
            }
        }
    }

    async Task RetrieveInformation()
    {
        string res = await Pharo.Execute("(RPackageOrganizer packageOrganizer packageNamed: '" + packageName + "') classes asString .");
        Debug.Log(res);
        classes = Regex.Replace(res, @"(a Set\()|\)|'|#|\n", "").Split(' ');
    }

    void GenerateCubes()
    {
        foreach (string classname in classes)
        {
            PharoClassCodeCube aClass = Instantiate(classCodeCubePrefab, transform);
            aClass.GetComponent<Renderer>().material.color = Color.magenta;
            aClass.className = classname;

            AddLine(aClass.gameObject, Color.magenta);
        }
    }

    void MoveChildren()
    {
        for (int i = 0; i < transform.childCount; i++)
        {
            CodeCube child = transform.GetChild(i).GetComponent<CodeCube>();
            child.transform.localRotation = Quaternion.Euler(0f, 0f, 0f);
            MoveLocallyTo(child.transform, new Vector3(2 * (i + 1), 0f, 0f), false);
            child.ScaleTo(child.transform.localScale / transform.localScale.x);
        }
    }

    void CleanCubes()
    {
        foreach (Transform child in transform)
        {
            Destroy(child.gameObject);
        }
    }

    async void Open()
    {
        loading = true;

        StartCoroutine(LoadingAnimation());
        await RetrieveInformation();

        GenerateCubes();
        RotateLocally(360);

        MoveChildren();

        loading = false;
        opened = true;
    }

    void Close()
    {
        opened = false;
        RotateLocally(-360);

        CleanCubes();
    }

    IEnumerator LoadingAnimation()
    {
        while (loading)
        {
            transform.Rotate(Time.deltaTime * rotationSpeed, 0f, 0f);
            yield return null;
        }
    }

    void AddLine(GameObject ob, Color c)
    {
        AFrameLine line = Instantiate(aFrameLinePrefab, transform.Find("Connecting Lines"));
        line.startObject = gameObject;
        line.endObject = ob;

        line.GetComponent<Renderer>().material.color = c;
    }

    public void OnHoverEnter(HoverEnterEventArgs args)
    {
        if (!isDragged)
        {
            CodeCubeText text = Instantiate(codeCubeTextPrefab, transform);
            text.GetComponent<TextMeshPro>().text = "package: " + packageName;
        }
    }

    public void OnHoverExit(HoverExitEventArgs args)
    {
        Transform text = transform.Find("CodeCubeText(Clone)");
        if (text) Destroy(text.gameObject);
    }
}