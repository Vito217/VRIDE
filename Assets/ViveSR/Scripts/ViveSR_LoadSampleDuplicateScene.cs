using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class ViveSR_LoadSampleDuplicateScene : MonoBehaviour {
    public bool SwitchSampleDuplicateScene;
    void LoadSampleDuplicate()
    {
        SceneManager.LoadScene("ViveSR_Sample_duplicate");
    }
    private void Update()
    {
        if (SwitchSampleDuplicateScene)
            LoadSampleDuplicate();
    }
}
