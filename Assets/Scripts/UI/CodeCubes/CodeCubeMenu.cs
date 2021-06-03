using UnityEngine;

public class CodeCubeMenu : MonoBehaviour
{
    void Update()
    {
        transform.forward = new Vector3(transform.forward.x, 0f, transform.forward.z);
    }
}
