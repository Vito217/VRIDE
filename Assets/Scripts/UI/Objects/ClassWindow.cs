using System.Collections;
using System.Collections.Generic;
using SaveAndLoad;

public class ClassWindow : BrowserWindow
{
    public override IEnumerator Coroutine()
    {
        foreach (KeyValuePair<string, (string classCode,
            List<(string methodName, string methodCode, string side)> classMethods)>
                keyVal in SaveAndLoadModule.sysData.data[name])
        {
            string className = keyVal.Key;
            string classCode = keyVal.Value.classCode;

            Instantiator.Instance.ClassObject(this, className, theBrowser.field,
                null, null, classCode, theBrowser);

            yield return null;
        }
        yield return base.Coroutine();
    }
}
