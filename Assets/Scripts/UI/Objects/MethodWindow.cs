using System.Collections;
using System.Collections.Generic;

public class MethodWindow : BrowserWindow
{
    public string package;
    public string side;

    public override IEnumerator Coroutine()
    {
        foreach ((string methodName, string methodCode, string side) methodAndCode in
                VRIDEController.sysData.data[package][name].classMethods)
        {
            if (side == methodAndCode.side)
            {
                string methodName = methodAndCode.methodName;
                string methodCode = methodAndCode.methodCode;

                Instantiator.Instance.MethodObject(transform, name, methodName,
                    theBrowser.field, methodCode, theBrowser);                
            }
            yield return null;
        }
    }
}
