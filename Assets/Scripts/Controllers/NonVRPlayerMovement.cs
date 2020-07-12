using UnityEngine;

public class NonVRPlayerMovement : MonoBehaviour
{
    public CharacterController controller;
    public VRIDEController vride_controller;
    public float speed = 1f;
    public float gravity = -10f;
    Vector3 velocity;

    void Start()
    {
        vride_controller = GetComponent<VRIDEController>();
    }

    void Update()
    {
        if (vride_controller.can_move)
        {
            float x = Input.GetAxis("Horizontal");
            float z = Input.GetAxis("Vertical");

            Vector3 move = transform.right * x + transform.forward * z;
            controller.Move(move * speed * Time.deltaTime);
            
        }
        velocity.y += gravity * Time.deltaTime;
        controller.Move(velocity * Time.deltaTime);
    }

    void OnControllerColliderHit(ControllerColliderHit hit)
    {
        if (hit.gameObject.tag == "Ground")
            velocity.y = 0f;
    }
}
