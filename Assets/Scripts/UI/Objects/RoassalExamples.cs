using System.Collections;
using UnityEngine;

public class RoassalExamples : InitializeBehaviour
{
    public RClassesWindow class_list;
    public RMethodsWindow methodList;
    public Color white, skyBlue, gray;
    private bool loadingClasses = true;

    public override IEnumerator innerStart()
    {
        ColorUtility.TryParseHtmlString("#FFFFFF", out white);
        yield return null;
        ColorUtility.TryParseHtmlString("#00FFFF", out skyBlue);
        yield return null;
        ColorUtility.TryParseHtmlString("#9D9D9D", out gray);
        yield return base.innerStart();
    }

    public override void innerBehaviour()
    {
        if (loadingClasses)
        {
            loadingClasses = false;
            class_list.Load();
        }
    }

    public override void onClose()
    {
        Destroy(gameObject);
    }
}
