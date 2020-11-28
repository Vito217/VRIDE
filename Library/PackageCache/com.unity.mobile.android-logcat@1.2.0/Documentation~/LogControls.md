### Log Controls

The log window contains multiple predefined columns, you can enable/disable them by right clicking on the columns.  
![Log Columns](images/log_columns.png)

**Copy**  
The selected logs can be copied to the clipboard. You can right click on the messages and select **Copy**.

**Save**  
The selected logs can be saved to file. You can right click on the messages and select **Save Selection**.

**Clear**  
You can clear all the logs by clicking the **Clear** button on the toolbar.

**Search**  
You can input text in the text field on the toolbar to search logs by text. You can also toggle **Regex** on to treat contents in the text field as a regex expression.
Note: The search is case sensitive, currently there's no way to perform case insensitive search, since the searching is performed via **adb logcat** command.
