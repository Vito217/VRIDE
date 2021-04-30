public class RoassalClass : RoassalObject
{
    public override void onSelect()
    {
        base.onSelect();
        if (roassal.class_list.last_selected != null)
            roassal.class_list.last_selected.onDeselect();
        roassal.class_list.last_selected = this;
        roassal.methodList.Load();
    }
}
