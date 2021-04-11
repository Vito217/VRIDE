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
        explorer.renameElem.interactable = true;

        base.onSelect();
    }
}
