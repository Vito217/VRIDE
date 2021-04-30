using System;
using System.Threading.Tasks;
using UnityEngine;

public abstract class RWindow : MonoBehaviour
{
    public RoassalExamples roassal;
    public RoassalObject last_selected = null;

    bool step1 = false;
    bool step2 = false;
    bool step3 = false;
    int index = 0;
    protected string[] contents;

    private int framesToWait = 2;
    private int currentFrame = 0;

    void Update()
    {
        if (step1) Clean();
        else if (step2) Query();
        else if (step3) Fill();
    }

    void Clean()
    {
        transform.localPosition = new Vector3(0f, 0f, 0f);
        foreach (Transform child in transform) Destroy(child.gameObject);
        step1 = false;
        step2 = true;
    }

    void Query()
    {
        step2 = false;
        index = 0;
        BeginQuery();
    }

    async void BeginQuery()
    {
        try
        {
            await InnerQuery(QueryKey());
            step3 = true;
        }
        catch (Exception e)
        {
            roassal.logText.text = "<color=#C63737>[Error] " + e.Message + "</color>";
        }
        roassal.Reactivate();
    }

    void Fill()
    {
        try
        {
            if (currentFrame == 0)
            {
                InnerFill(contents[index]);
                index++;
            }
            currentFrame = (currentFrame + 1) % framesToWait;
        }
        catch
        {
            step3 = false;
        }
    }

    public void Load()
    {
        roassal.DeactivateTemporarily();
        step1 = true;
    }

    public abstract string QueryKey();

    public abstract Task InnerQuery(string key);

    public abstract void InnerFill(string content);
}
