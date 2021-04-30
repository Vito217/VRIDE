public class ExplorerDirectory : ExplorerObject
{
    public override void onSelect()
    {
        if (explorer.lastSelected != null) explorer.lastSelected.onDeselect();
        explorer.lastSelected = this;

        explorer.newDir.interactable = true;
        explorer.newFile.interactable = true;
        explorer.deleteElem.interactable = true;
        explorer.editElem.interactable = false;
        explorer.renameElem.interactable = true;

        base.onSelect();
    }
}
