public enum FingerType
{
    None,
    Thumb,
    Index,
    Middle,
    Ring,
    Pinky
}

public class Finger
{
    public FingerType type = FingerType.None;
    public float current = 0.0f;
    public float target = 0.0f;

    public Finger(FingerType type)
    {
        this.type = type;
    }
}
