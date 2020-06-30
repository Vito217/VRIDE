using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;
using TMPro;

public class LoadingScreen : MonoBehaviour
{
    AsyncOperation loadingOperation;
    public Slider progressBar;

    void Start()
    {
        loadingOperation = SceneManager.LoadSceneAsync("MainScene");
    }

    void Update()
    {
        progressBar.value = Mathf.Clamp01(loadingOperation.progress / 0.9f);
    }
}
