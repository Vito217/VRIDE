using UnityEngine;

public class VRKeyboard : MonoBehaviour
{
    public InitializeBehaviour window;
    public bool hidden;
    Vector3 originalPosition;

    void Start()
    {
        originalPosition = transform.localPosition;
        if (hidden)
            transform.position = new Vector3(
                transform.position.x,
                -10f,
                transform.position.z
                );
        if (name == "Virtual Keyboard")
            gameObject.SetActive(false);
    }

    void Update()
    {
        if (hidden)
            transform.position = new Vector3(
                transform.position.x,
                -10f,
                transform.position.z
                );
        else
            transform.localPosition = originalPosition;
    }
}
