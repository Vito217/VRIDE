public class DoItKey : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                Playground p = keyboard.window as Playground;
                p.PharoDo();
            }
            catch { }
        }
    }
}
