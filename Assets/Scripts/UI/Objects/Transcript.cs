using LoggingModule;
using System.Collections;

public class Transcript : InitializeBehaviour
{
    void Update()
    {
        if (initializing)
            initializeAnimation();
        else if (dragging)
            dragAction();
        else
            field.text = VRIDEController.transcriptContents;
    }

    public override void onClose()
    {
        player.transcripts.Remove(this);
        InteractionLogger.Discount("Transcript");
        Destroy(gameObject);
    }

    public override IEnumerator Coroutine()
    {
        paintPanels();
        yield return null;
    }
}
