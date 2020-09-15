using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RoassalClass : RoassalObject
{
    public override async void onSelect()
    {
        base.onSelect();
        if (roassal.class_list.last_selected != null)
            roassal.class_list.last_selected.onDeselect();
        roassal.class_list.last_selected = this;
        roassal.methodList.Load();
    }
}
