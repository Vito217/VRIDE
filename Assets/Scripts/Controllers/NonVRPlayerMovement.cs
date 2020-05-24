using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NonVRPlayerMovement : MonoBehaviour
{
    public CharacterController controller;
    public float speed = 1f;
    public float gravity = -10f;
    public bool can_move = true;
    Vector3 velocity;

    void Start()
    {

    }

    void Update()
    {
        if (can_move)
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
        if (hit.gameObject.tag == "Ground") {
            velocity.y = 0f;
        }
    }
}
