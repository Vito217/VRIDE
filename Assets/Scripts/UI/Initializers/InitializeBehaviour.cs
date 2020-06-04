using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class InitializeBehaviour : MonoBehaviour
{
    public ToolbarBehaviour toolbar;
    public float speed = 8.0f;
    public bool initializing = false;
    public Vector3 new_pos;

    void Update()
    {
        if (initializing)
            initializeAnimation();
    }

    void initializeAnimation()
    {
        transform.position = Vector3.MoveTowards(
            transform.position,
            new_pos,
            speed
        );
        if (transform.position == new_pos)
            initializing = false;
    }

    public virtual void Initialize(Vector3 init_pos, Vector3 final_pos, Vector3 forward, GameObject player)
    {
        transform.position = init_pos;
        transform.forward = forward;
        new_pos = final_pos;
        toolbar.player = player;
        initializing = true;
    }
}
