using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Keyboards : MonoBehaviour
{
    private float globalPositionY = 0f;

    void Start()
    {
        globalPositionY = transform.position.y;
    }

    void Update()
    {
        transform.position = new Vector3(
            transform.position.x, globalPositionY, transform.position.z);
        transform.localRotation = Quaternion.Euler(
            0, 0, 0);
    }
}
