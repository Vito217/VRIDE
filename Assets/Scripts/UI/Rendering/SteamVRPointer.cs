using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Security.Cryptography;
using UnityEngine;

public class SteamVRPointer : MonoBehaviour
{
    public float defaultLength = 5.0f;
    public GameObject m_Dot;
    public SteamVRInputModule m_InputModule;
    private LineRenderer m_LineRenderer;

    // Start is called before the first frame update
    void Awake()
    {
        m_LineRenderer = GetComponent<LineRenderer>();
    }

    // Update is called once per frame
    void Update()
    {
        UpdateLine();
    }

    void UpdateLine()
    {
        float targetLength = defaultLength;
        RaycastHit hit = CreateRaycast(targetLength);
        Vector3 endPosition = transform.position + (transform.forward * targetLength);
        if (hit.collider != null)
            endPosition = hit.point;
        m_Dot.transform.position = endPosition;
        m_LineRenderer.SetPosition(0, transform.position);
        m_LineRenderer.SetPosition(1, endPosition);
    }

    RaycastHit CreateRaycast(float length)
    {
        RaycastHit hit;
        Ray ray = new Ray(transform.position, transform.forward);
        Physics.Raycast(ray, out hit, defaultLength);

        return hit;
    }
}
