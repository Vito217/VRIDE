public class InspectItKey : VRKey
{
    public override void OnClick()
    {
        if (keyboard.window != null && !keyboard.window.loadingWheel.activeSelf)
        {
            try
            {
                Playground p = keyboard.window as Playground;
                _ = p.PharoInspect();
            }
            catch { }
        }
    }
}
