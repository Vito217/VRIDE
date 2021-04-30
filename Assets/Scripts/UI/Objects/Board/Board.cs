using LoggingModule;

public class Board : InitializeBehaviour
{
    public override void onClose()
    {
        InteractionLogger.Discount("Board", GetInstanceID().ToString());

        base.onClose();
    }
}
