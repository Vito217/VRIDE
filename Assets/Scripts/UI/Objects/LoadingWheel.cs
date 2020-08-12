using UnityEngine;

public class LoadingWheel : MonoBehaviour
{
    float grades = 0.0f;

    void Update()
    {
        grades = (grades + 1.0f) % 360;
        transform.localEulerAngles = new Vector3(0, 0, -grades);
    }
}
