public class BrowserMethod : BrowserObject
{
    public override void innerStart()
    {
        if(sourceCode == "")
        {
            sourceCode =
                "<b>messageSelectorAndArgumentNames</b>\n" +
                    "    | temporary variable names |\n" +
                    "    statements";
        }
    }

    public override void onSelect()
    {
        theBrowser.field.text = sourceCode;
    }
}
