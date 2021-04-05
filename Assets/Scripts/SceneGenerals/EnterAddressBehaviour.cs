using UnityEngine;
using TMPro;
using UnityEngine.SceneManagement;
using SaveAndLoad;
using System.IO;
using LoggingModule;
using System.Collections;

public class EnterAddressBehaviour : InitializeBehaviour
{
    public TMP_InputField username;
    public GameObject aboutSection, mainSection;

    public override IEnumerator innerStart()
    {
        Initialize();
        aboutSection.transform.position = transform.TransformPoint(GetComponent<RectTransform>().sizeDelta.x, 0f, 0f);
        return base.innerStart();
    }

    public void OnButtonClick()
    {
        OnSubmit();
    }

    public void OnSubmit()
    {
        loadingWheel.SetActive(true);
        SaveAndLoadModule.username = username.text;
        SaveAndLoadModule.sessionPath = Path.Combine(Application.persistentDataPath, username.text + ".data");
        InteractionLogger.persistentPath = Path.Combine(Application.persistentDataPath, username.text + "_log.txt");
        SceneManager.LoadSceneAsync("MainScene");
    }

    public void OnExit()
    {
        Application.Quit();
    }
}
