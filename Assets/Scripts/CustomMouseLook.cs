using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CustomMouseLook : MonoBehaviour
{
    public float sensitivity = 100f;
    public Transform playerBody;
    public float xRotation = 0f;
    public float range = 100f;

    public GameObject browser_prefab;
    public GameObject playground_prefab;
    public GameObject spawner_prefab;
    public GameObject spawner;
    private int sp_opacity; 
    
    // Start is called before the first frame update
    void Start()
    {
        spawner = Instantiate(spawner_prefab);
        spawner.transform.SetParent(transform, false);
        spawner.SetActive(false);
        sp_opacity = 0;
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

                if (spawner.activeSelf)
                {
                    var r = spawner.GetComponent<Renderer>();
                    float alpha = Mathf.Abs(Mathf.Sin(Mathf.Deg2Rad * sp_opacity));
                    r.material.color = new Color(r.material.color.r, r.material.color.g, r.material.color.b, alpha);
                    sp_opacity = (sp_opacity + 4) % 360;
                    spawner.transform.position = hit.point;
                }

                if (Input.GetKeyDown("q")) {
                    GameObject new_browser = Instantiate(browser_prefab);
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

        if (Input.GetKeyUp(KeyCode.LeftControl))
            spawner.SetActive(false);

        else if (Input.GetKeyDown(KeyCode.LeftControl))
            spawner.SetActive(true);
    }
}
