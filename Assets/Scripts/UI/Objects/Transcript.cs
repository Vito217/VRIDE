using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using LoggingModule;
using System;

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
