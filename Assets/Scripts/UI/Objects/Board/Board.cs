using LoggingModule;

public class Board : InitializeBehaviour
{
    public override void onClose()
    {
        InteractionLogger.Discount("Board", GetInstanceID().ToString());

        /**
        Transform area = transform.Find("Panel/Area");
        area.GetComponent<BoxCollider>().enabled = false;
        foreach (Transform t in area) 
        {
            if (t.GetComponent<Canvas>())
                t.GetComponent<Canvas>().enabled = true;

            t.SetParent(null);
        }
        **/

        base.onClose();
    }
}
