public class BrowserPackage : BrowserObject
{
    public override void onSelect()
    {
        if (theBrowser.package_list.last_selected != null)
            theBrowser.package_list.last_selected.onDeselect();
        if (theBrowser.class_list.last_selected != null)
            theBrowser.class_list.last_selected.onDeselect();
        if (theBrowser.methodList.last_selected != null)
            theBrowser.methodList.last_selected.onDeselect();
        theBrowser.package_list.last_selected = this;
        theBrowser.class_list.gameObject.SetActive(true);
        theBrowser.methodList.gameObject.SetActive(false);
        theBrowser.classFilter.interactable = true;
        theBrowser.methodFilter.interactable = false;
        theBrowser.class_list.Load();
        theBrowser.packageRemover.interactable = true;
        base.onSelect();
    }
}
