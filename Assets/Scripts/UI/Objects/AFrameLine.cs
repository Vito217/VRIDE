using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AFrameLine : MonoBehaviour
{
    public GameObject startObject;
    public GameObject endObject;

    public Vector3 start;
    public Vector3 end;

    void Update()
    {
        if (startObject != null)
            start = startObject.transform.position;

        if (endObject != null)
            end = endObject.transform.position;

        Vector3 lineDir = end - start;
        Vector3 baseDir = new Vector3(lineDir.x, start.y, lineDir.z);
        Vector3 center = (start + end) * 0.5f;
        float mag = lineDir.magnitude / (transform.parent.parent.localScale.x);

        transform.position = center;
        transform.localScale = new Vector3(.02f, .02f, mag);
        transform.LookAt(endObject.transform);
    }
}
