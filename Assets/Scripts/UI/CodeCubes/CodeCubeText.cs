using UnityEngine;

public class CodeCubeText : MonoBehaviour
{
    void Update()
    {
        transform.forward = new Vector3(transform.forward.x, 0f, transform.forward.z);
    }
}
