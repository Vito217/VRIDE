using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MouseLook : MonoBehaviour
{
    public float sensitivity = 100f;
    public Transform playerBody;
    public float xRotation = 0f;
    public float range = 100f;

    public GameObject browser_prefab;
    public GameObject playground_prefab;
    
    // Start is called before the first frame update
    void Start()
    {
        // Cursor.lockState = CursorLockMode.Locked;
    }

    // Update is called once per frame
    void Update()
    {
        
        Cursor.lockState = CursorLockMode.Locked;
        Cursor.lockState = CursorLockMode.None;

        float mouseX = Input.GetAxis("Mouse X") * sensitivity * Time.deltaTime;
        float mouseY = Input.GetAxis("Mouse Y") * sensitivity * Time.deltaTime;
        xRotation -= mouseY;
        xRotation = Mathf.Clamp(xRotation, -90f, 90f);

        transform.localRotation = Quaternion.Euler(xRotation, 0f, 0f);
        playerBody.Rotate(Vector3.up * mouseX);

        
        if (Input.GetKey(KeyCode.LeftControl)) {
            RaycastHit hit;
            if (Physics.Raycast(transform.position, transform.forward, out hit, range)) {
                //KeyInput k = (KeyInput) hit.transform.gameObject.GetComponent("KeyInput");
                //code.text += k.value;

                if (Input.GetKeyDown("q")) {
                    GameObject new_browser = Instantiate(browser_prefab);
                    //new_browser.transform.Find("Classes").Find("Panel").gameObject.GetComponent<Image>().color = Random.ColorHSV();
                    new_browser.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_browser.transform.position = hit.point;
                }
                else if (Input.GetKeyDown("e"))
                {
                    GameObject new_playground = Instantiate(playground_prefab);
                    new_playground.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_playground.transform.position = hit.point;
                }
            }
        }
    }
}
