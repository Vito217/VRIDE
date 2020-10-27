using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class ViveSR_SRworkManager : MonoBehaviour {

    static ViveSR_SRworkManager instance;
    void Awake() {
        if (instance == null)
        {
            instance = this;
            DontDestroyOnLoad(this);
            name = "[SRwork_FrameWork]";
        }
        else if (this != instance)
        {
            Destroy(gameObject);
        }
    }
}
