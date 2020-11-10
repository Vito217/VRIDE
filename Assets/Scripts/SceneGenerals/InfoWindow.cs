using UnityEngine;

/// <summary>
/// Used in About section and Tasks list
/// </summary>
public class InfoWindow : MonoBehaviour
{
    public Transform playerCamera;

    void Start()
    {
        if(playerCamera != null)
            transform.position = new Vector3(
                transform.position.x, playerCamera.position.y + 1f, transform.position.z);
    }
}
