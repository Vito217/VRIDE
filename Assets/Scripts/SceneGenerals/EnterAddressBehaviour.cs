﻿using System;
using UnityEngine;
using TMPro;
using PharoModule;
using UnityEngine.SceneManagement;

public class EnterAddressBehaviour : MonoBehaviour
{
    public TMP_InputField input;
    public GameObject loadingWheel;
    public TextMeshProUGUI errorText;

    void Start()
    {
        input.onSubmit.AddListener(OnSubmit);
    }

    public void OnButtonClick()
    {
        OnSubmit(input.text);
    }

    public async void OnSubmit(string line)
    {
        try
        {
            loadingWheel.SetActive(true);
            Pharo.IP = line;
            string res = await Pharo.Execute("Object new .");
            if (!res.Contains("an Object"))
                throw new Exception("Not a Pharo response");
            SceneManager.LoadSceneAsync("MainScene");
        }
        catch
        {
            errorText.text = "Not Found: Make sure the address corresponds to a Pharo server.";
            loadingWheel.SetActive(false);
        }
    }

    public void OnExit()
    {
        Application.Quit();
    }
}
