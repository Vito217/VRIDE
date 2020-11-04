using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovingBar : MonoBehaviour
{
    void OnCollisionStay(Collision collision)
    {
        Transform col = collision.transform;
        Vector3 dir = transform.position - col.position;
        transform.position += new Vector3(dir.x, dir.y, 0f);
    }
}
