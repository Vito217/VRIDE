using UnityEngine;

public class BoardArea : MonoBehaviour
{
    void LateUpdate()
    {
        foreach(Transform child in transform)
        {
            child.transform.forward = transform.forward;
            if (child.GetComponent<Canvas>())
            {
                child.GetComponent<Canvas>().enabled = false;
                child.GetComponent<Canvas>().enabled = true;
            }
        }
    }

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
            collider.transform.SetParent(transform);
    }
}
