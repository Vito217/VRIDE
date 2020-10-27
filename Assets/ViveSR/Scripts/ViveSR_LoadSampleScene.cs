using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class ViveSR_LoadSampleScene : MonoBehaviour {
    public bool SwitchSampleScene;
    void LoadSample()
    {
        SceneManager.LoadScene("ViveSR_Sample");
    }
    private void Update()
    {
        if (SwitchSampleScene)
            LoadSample();
    }
}
