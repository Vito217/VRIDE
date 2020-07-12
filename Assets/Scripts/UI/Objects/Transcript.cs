using LoggingModule;

public class Transcript : InitializeBehaviour
{
    public override void onClose()
    {
        player.transcripts.Remove(this);
        InteractionLogger.Discount("Transcript");
        Destroy(gameObject);
    }

    public override void innerBehaviour()
    {
        field.text = VRIDEController.transcriptContents;
    }
}
