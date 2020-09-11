using UnityEngine;

public class NonVRPlayerMovement : MonoBehaviour
{
    public VRIDEController vride_controller;
    public float speed = 1f;
    public float gravity = -10f;
    Vector3 velocity;

    void Update()
    {
        if (vride_controller.can_move)
        {
            float x = Input.GetAxis("Horizontal");
            float z = Input.GetAxis("Vertical");

            Vector3 move = transform.right * x + transform.forward * z;
            transform.position += move * speed * Time.deltaTime;
        }
    }
}
