public class BrowserMethod : BrowserObject
{
    public override void innerStart()
    {
        if(sourceCode == "")
        {
            sourceCode =
                "messageSelectorAndArgumentNames\n" +
                    "    | temporary variable names |\n" +
                    "    statements";
        }
    }

    public override void onSelect()
    {
        theBrowser.field.text = sourceCode;
    }
}
