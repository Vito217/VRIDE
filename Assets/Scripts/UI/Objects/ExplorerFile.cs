using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExplorerFile : ExplorerObject
{
    public override void onSelect()
    {
        if (explorer.lastSelected != null) explorer.lastSelected.onDeselect();
        explorer.lastSelected = this;

        explorer.newDir.interactable = false;
        explorer.newFile.interactable = false;
        explorer.deleteElem.interactable = true;
        explorer.editElem.interactable = true;

        base.onSelect();
    }
}
