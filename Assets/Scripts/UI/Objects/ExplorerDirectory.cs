using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExplorerDirectory : ExplorerObject
{
    public override void onSelect()
    {
        base.onSelect();

        if (explorer.lastSelected != null) explorer.lastSelected.onDeselect();
        explorer.lastSelected = this;

        explorer.newDir.interactable = true;
        explorer.newFile.interactable = true;
        explorer.deleteElem.interactable = true;
        explorer.editElem.interactable = false;
    }
}
