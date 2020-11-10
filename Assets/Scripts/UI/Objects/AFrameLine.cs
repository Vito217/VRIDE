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
            start = transform.parent.InverseTransformPoint(
                startObject.transform.position);

        if (endObject != null)
            end = transform.parent.InverseTransformPoint(
                endObject.transform.position);

        Vector3 lineDir = end - start;
        Vector3 baseDir = new Vector3(lineDir.x, start.y, lineDir.z);
        Vector3 center = (start + end) * 0.5f;
        float mag = lineDir.magnitude;

        transform.localPosition = center;
        transform.localScale = new Vector3(.02f, .02f, mag);
        transform.LookAt(transform.parent.TransformPoint(end));
    }
}
