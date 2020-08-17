using LoggingModule;
using SaveAndLoad;

public class Transcript : InitializeBehaviour
{
    public override void onClose()
    {
        SaveAndLoadModule.transcripts.Remove(this);
        InteractionLogger.Discount("Transcript");
        Destroy(gameObject);
    }

    public override void innerBehaviour()
    {
        field.text = SaveAndLoadModule.transcriptContents;
    }
}
