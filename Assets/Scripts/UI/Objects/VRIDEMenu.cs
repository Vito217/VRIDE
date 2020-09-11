using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class VRIDEMenu : InitializeBehaviour
{
    public Button playgroundGenerator;
    public Button browserGenerator;
    public Button transcriptGenerator;
    public Button quit;

    public void Reset()
    {
        playgroundGenerator.onClick.RemoveAllListeners();
        browserGenerator.onClick.RemoveAllListeners();
        transcriptGenerator.onClick.RemoveAllListeners();
        quit.onClick.RemoveAllListeners();
    }
}
