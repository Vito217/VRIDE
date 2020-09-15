using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RoassalMethod : RoassalObject
{
    public override async void onSelect()
    {
        base.onSelect();
        roassal.DeactivateTemporarily();
        if (roassal.methodList.last_selected != null)
            roassal.methodList.last_selected.onDeselect();
        roassal.methodList.last_selected = this;

        // Load a visualization
        string aClass = roassal.class_list.last_selected.name;
        string code = "(" + aClass + ">>#" + name + ") sourceCode .";

        roassal.Reactivate();
    }
}
