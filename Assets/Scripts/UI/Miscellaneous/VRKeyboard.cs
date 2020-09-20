using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using TMPro;

public class VRKeyboard : MonoBehaviour
{
    public InitializeBehaviour window;
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
            75, 0, 0);
    }
}
