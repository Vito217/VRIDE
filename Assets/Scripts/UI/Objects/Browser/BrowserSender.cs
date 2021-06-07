using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BrowserSender : BrowserObject
{
    public override void onSelect()
    {
        if (theBrowser.senderList.last_selected != null)
            theBrowser.senderList.last_selected.onDeselect();
        theBrowser.senderList.last_selected = this;
        base.onSelect();

        string className = name.Split('>')[0];

        PharoClassCodeCube classCodeCube = Instantiate(theBrowser.pharoClassCodeCubePrefab);
        classCodeCube.transform.position = transform.position;
        classCodeCube.className = className;
        classCodeCube.transform.position = transform.position;
        classCodeCube.transform.forward = transform.forward;
    }
}
