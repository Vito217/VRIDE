﻿using System.Collections;
using UnityEngine;

public class BrowserWindow : MonoBehaviour
{
    public Browser theBrowser;
    public BrowserObject last_selected = null;

    void Start()
    {
        StartCoroutine(Coroutine());
    }

    public BrowserObject getLastSelected()
    {
        return last_selected;
    }

    public void setLastSelected(BrowserObject o)
    {
        last_selected = o;
    }

    public virtual IEnumerator Coroutine() {
        yield return null;
    }
}
