using System.Collections;
using UnityEngine;
using UnityEngine.Networking;

public class DesktopWindowsExplorer : InitializeBehaviour
{
    public Transform contentList;

    public static string streamerIP;

    [HideInInspector]
    public Color white, skyBlue, gray;

    [HideInInspector]
    public DesktopWindowObject lastSelected;

    public override IEnumerator innerStart()
    {
        ColorUtility.TryParseHtmlString("#FFFFFF", out white);
        ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue);
        ColorUtility.TryParseHtmlString("#9D9D9D", out gray);

        StartCoroutine(Fill());

        yield return base.innerStart();
    }

    IEnumerator Fill()
    {
        DeactivateTemporarily();
        yield return null;

        Instantiator.Instance.DesktopWindowObject("desktop", "Desktop", this);
        yield return null;

        using (UnityWebRequest uwr = UnityWebRequest.Get(streamerIP))
        {
            yield return uwr.SendWebRequest();
            try
            {
                string result = uwr.downloadHandler.text;
                string[] pairList = result.Split(';');
                foreach(string pair in pairList)
                {
                    string[] values = pair.Split('=');
                    string hwnd = values[0];
                    string windowName = values[1];

                    Instantiator.Instance.DesktopWindowObject(hwnd, windowName, this);
                    yield return null;
                }
            }
            finally
            {
                Reactivate();
            }
        }
    }
}
