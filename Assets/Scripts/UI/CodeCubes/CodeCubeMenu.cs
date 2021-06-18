using UnityEngine;

public class CodeCubeMenu : MonoBehaviour
{
    //void Start()
    //{
    //    transform.forward = -Camera.main.transform.forward;
    //}

    void Update()
    {
        //transform.forward = new Vector3(transform.forward.x, 0f, transform.forward.z);
        transform.forward = Camera.main.transform.forward;
    }

}
