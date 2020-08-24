using System;
using System.Threading.Tasks;
using UnityEngine;

public abstract class BrowserWindow : MonoBehaviour
{
    public Browser theBrowser;
    public BrowserObject last_selected = null;

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

    public void Clean()
    {
        if (transform.childCount > 0)
            Destroy(transform.GetChild(0).gameObject);
        else
        {
            step1 = false;
            step2 = true;
        }
    }

    public void Query()
    {
        step2 = false;
        index = 0;
        BeginQuery();
    }

    public async void BeginQuery()
    {
        try
        {
            await InnerQuery(QueryKey());            
            step3 = true;
        }
        catch (Exception e)
        {
            theBrowser.field.text += " -> [Error] " + e.Message;
        }
        theBrowser.Reactivate();
    }

    public void Fill()
    {
        try
        {
            if(currentFrame == 0)
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
        theBrowser.DeactivateTemporarily();
        step1 = true; 
    }

    public abstract string QueryKey();

    public abstract Task InnerQuery(string key);

    public abstract void InnerFill(string content);
}
