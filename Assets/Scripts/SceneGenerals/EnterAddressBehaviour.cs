using System;
using System.Text.RegularExpressions;
using UnityEngine;
using TMPro;
using PharoModule;
using UnityEngine.SceneManagement;

/// <summary>
/// Used for checking connection with the Pharo server
/// </summary>
public class EnterAddressBehaviour : MonoBehaviour
{
    public TMP_InputField address;
    public TMP_InputField port;
    public GameObject loadingWheel, aboutSection, mainSection;
    public TextMeshProUGUI errorText;
    public void OnButtonClick()
    {
        OnSubmit();
    }

    /// <summary>
    /// Sends a ping to the given IP
    /// </summary>
    public async void OnSubmit()
    {
        try
        {
            loadingWheel.SetActive(true);

            Pharo.IP = Regex.Replace(
                "http://" + address.text + ":" + port.text + "/repl",
                @"\n|\s|\t",
                @""
            );

            string res = await Pharo.Execute("Author uniqueInstance fullName: 'VRIDE User'.");
            if (!res.Contains("an Author"))
                throw new Exception("Not a Pharo response");
            SceneManager.LoadSceneAsync("MainScene");
        }
        catch
        {
            errorText.text = "Not Found: Make sure the address corresponds to a Pharo server.";
            address.ActivateInputField();
            loadingWheel.SetActive(false);
        }
    }

    public void OnExit()
    {
        Application.Quit();
    }
}
