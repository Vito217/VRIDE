using System.Collections;
using UnityEngine;

public class AFrameLine : MonoBehaviour
{
    public GameObject startObject;
    public GameObject endObject;

    public Vector3 start;
    public Vector3 end;

    void Start()
    {
        StartCoroutine(UpdateLine());
    }

    IEnumerator UpdateLine()
    {
        while (true)
        {
            if (startObject != null && endObject != null && transform.parent != null)
            {
                start = transform.parent.InverseTransformPoint(
                    startObject.transform.position);

                yield return null;

                end = transform.parent.InverseTransformPoint(
                    endObject.transform.position);

                yield return null;

                transform.LookAt(transform.parent.TransformPoint(end));

                yield return null;

                Vector3 lineDir = end - start;
                Vector3 center = (start + end) * 0.5f;

                transform.localPosition = center;

                var scale = transform.localScale;
                scale.z = lineDir.magnitude;
                transform.localScale = scale;
            }
            else
            {
                break;
            }
            yield return null;
        }
        Destroy(gameObject);
    }
}
