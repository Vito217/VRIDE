using UnityEngine;

public class BoardArea : MonoBehaviour
{
    void OnTriggerStay(Collider collider)
    {
        bool isWindow = collider.name.Contains("Playground") ||
                        collider.name.Contains("Browser") ||
                        collider.name.Contains("Transcript") ||
                        collider.name.Contains("Inspector") ||
                        collider.name.Contains("AFrame") ||
                        collider.name.Contains("Graph") ||
                        collider.name.Contains("FileExplorer") ||
                        collider.name.Contains("CodeCube") ||
                        collider.name.Contains("WindowCube") ||
                        collider.name.Contains(".py");

        if (isWindow && collider.transform.parent == null)
        {
            collider.transform.forward = transform.forward;
            collider.transform.SetParent(transform);

            if (collider.GetComponent<Canvas>())
            {
                collider.GetComponent<Canvas>().enabled = false;
                collider.GetComponent<Canvas>().enabled = true;
            }
        }
    }

    void OnTriggerEnter(Collider collider)
    {
        bool isWindow = collider.name.Contains("Playground") ||
                        collider.name.Contains("Browser") ||
                        collider.name.Contains("Transcript") ||
                        collider.name.Contains("Inspector") ||
                        collider.name.Contains("AFrame") ||
                        collider.name.Contains("Graph") ||
                        collider.name.Contains("FileExplorer") ||
                        collider.name.Contains("CodeCube") ||
                        collider.name.Contains("WindowCube") ||
                        collider.name.Contains(".py");

        if (isWindow && collider.transform.parent == null)
        {
            collider.transform.forward = transform.forward;
            collider.transform.SetParent(transform);

            if (collider.GetComponent<Canvas>())
            {
                collider.GetComponent<Canvas>().enabled = false;
                collider.GetComponent<Canvas>().enabled = true;
            }
        }
    }
}
