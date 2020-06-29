using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using LoggingModule;

public class Transcript : InitializeBehaviour
{
    public override void onClose()
    {
        player.transcripts.Remove(this);
        InteractionLogger.Discount("Transcript");
        Destroy(gameObject);
    }
}
