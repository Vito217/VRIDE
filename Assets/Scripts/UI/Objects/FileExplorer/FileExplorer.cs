using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;
using SaveAndLoad;
using TMPro;
using System;
using LoggingModule;

public class FileExplorer : InitializeBehaviour
{
    public Transform contentList;

    [HideInInspector]
    public Color white, skyBlue, gray;

    public Button newDir;
    public Button newFile;
    public Button deleteElem;
    public Button editElem;
    public Button renameElem;

    public TMP_Dropdown fileExtension;
    public TMP_InputField filename;
    public TMP_InputField dirname;
    public TMP_InputField newName;

    public GameObject fileCreationPanel;
    public GameObject dirCreationPanel;
    public GameObject renamePanel;

    [HideInInspector]
    public ExplorerObject lastSelected;

    public override IEnumerator innerStart()
    {
        ColorUtility.TryParseHtmlString("#FFFFFF", out white);
        ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue);
        ColorUtility.TryParseHtmlString("#9D9D9D", out gray);

        Fill();

        yield return base.innerStart();
    }

    IEnumerator HandleDirectory(string dir, string tab)
    {
        string[] paths = Regex.Split(dir, @"/|\\");
        string name = paths[paths.Length - 1];

        GameObject newDir = Instantiator.Instance.ExplorerDirectory(tab + name + "/", contentList);
        newDir.GetComponent<ExplorerDirectory>().explorer = this;
        newDir.GetComponent<ExplorerDirectory>().fullPath = dir;

        yield return null;

        string[] subdirs = Directory.GetDirectories(dir);
        string[] files = Directory.GetFiles(dir);

        if (files.Length > 0)
            foreach (string file in files)
                yield return HandleFile(file, "\t" + tab);

        if (subdirs.Length > 0)
            foreach (string subdir in subdirs)
                yield return HandleDirectory(subdir, "\t" + tab);
    }

    IEnumerator HandleFile(string file, string tab)
    {
        string[] paths = Regex.Split(file, @"/|\\");
        string name = paths[paths.Length - 1];

        GameObject newFile = Instantiator.Instance.ExplorerFile(tab + name, contentList);
        newFile.GetComponent<ExplorerFile>().explorer = this;
        newFile.GetComponent<ExplorerFile>().fullPath = file;

        yield return null;
    }

    public void FileCreation() { fileCreationPanel.SetActive(!fileCreationPanel.activeSelf); }
    public void DirectoryCreation() { dirCreationPanel.SetActive(!dirCreationPanel.activeSelf); }
    public void Renaming() { renamePanel.SetActive(!renamePanel.activeSelf); }

    public void GenerateNewFile()
    {
        if (!String.IsNullOrWhiteSpace(filename.text))
        {
            string extension = "";
            if (fileExtension.value == 0) //Python
                extension = ".py";

            string filePath = Path.Combine(lastSelected.fullPath, filename.text + extension);
            using FileStream fs = File.Create(filePath);
            fileCreationPanel.SetActive(false);
            RestartContent();
        }
    }

    public void GenerateNewDirectory()
    {
        if (!String.IsNullOrWhiteSpace(dirname.text))
        {
            string dirPath = Path.Combine(lastSelected.fullPath, dirname.text);
            Directory.CreateDirectory(dirPath);

            dirCreationPanel.SetActive(false);

            RestartContent();
        }
    }

    public void DeleteElement()
    {
        if (File.Exists(lastSelected.fullPath))
            File.Delete(lastSelected.fullPath);
        else
            Directory.Delete(lastSelected.fullPath);

        RestartContent();
    }

    public void EditFile()
    {
        PythonEditor editor;
        GameObject editorObject = GameObject.Find(Path.GetFileName(lastSelected.fullPath));

        if(editorObject != null)
        {
            editor = editorObject.GetComponent<PythonEditor>();
        }
        else
        {
            editor = Instantiator.Instance.PythonEditor();
            editor.fullpath = lastSelected.fullPath;
            editor.pythonCode.text = File.ReadAllText(editor.fullpath);
        }

        float width = GetComponent<RectTransform>().sizeDelta.x * transform.Find("Panel").GetComponent<RectTransform>().localScale.x;
        editor.transform.Find("Panel").GetComponent<RectTransform>().localScale = transform.Find("Panel").GetComponent<RectTransform>().localScale;
        editor.GetComponent<RectTransform>().sizeDelta = GetComponent<RectTransform>().sizeDelta;
        editor.transform.position = transform.TransformPoint(width, 0f, 0f);
        editor.transform.forward = transform.forward;

        SaveAndLoadModule.pyEditors.Add(editor);
    }

    public void RenameSelectedElement()
    {
        if (!String.IsNullOrWhiteSpace(newName.text))
        {
            string baseFolder = Path.GetDirectoryName(lastSelected.fullPath);
            string previousElement = lastSelected.fullPath;
            string newElement = Path.Combine(baseFolder, newName.text);

            if (lastSelected.GetType() == typeof(ExplorerDirectory))
            {
                Directory.CreateDirectory(newElement);
                Directory.Move(previousElement, newElement);
                Directory.Delete(previousElement);
            }
            else
            {
                newElement += Path.GetExtension(previousElement);

                File.Move(previousElement, newElement);
                File.Delete(previousElement);

                GameObject editor = GameObject.Find(Path.GetFileName(previousElement));
                if (editor != null)
                {
                    editor.name = newName.text;
                    editor.GetComponent<PythonEditor>().filename.text = newName.text;
                    editor.GetComponent<PythonEditor>().fullpath = newElement;
                }
            }

            renamePanel.SetActive(false);

            RestartContent();
        }
    }

    void RestartContent()
    {
        Clean();
        Fill();
    }

    void Fill()
    {
        string path = Path.Combine(Application.persistentDataPath, SaveAndLoadModule.username);

        if (!Directory.Exists(path))
            Directory.CreateDirectory(path);

        StartCoroutine(HandleDirectory(path, ""));

        newDir.interactable = false;
        newFile.interactable = false;
        deleteElem.interactable = false;
        editElem.interactable = false;
        renameElem.interactable = false;
    }

    void Clean()
    {
        foreach (Transform child in contentList)
            Destroy(child.gameObject);
    }

    public override void onClose()
    {
        InteractionLogger.Discount("FileExplorer", GetInstanceID().ToString());

        base.onClose();
    }
}
