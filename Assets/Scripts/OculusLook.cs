using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using OVRTouchSample;

public class OculusLook : MonoBehaviour
{
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
        if (Input.GetKey(KeyCode.LeftControl) || 
            OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger, OVRInput.Controller.Touch) > 0 ||
            OVRInput.Get(OVRInput.Axis1D.SecondaryIndexTrigger, OVRInput.Controller.Touch) > 0)
        {
            RaycastHit hit;

            if (Physics.Raycast(transform.position, transform.forward, out hit, range))
            {

                if (spawner.activeSelf)
                {
                    var r = spawner.GetComponent<Renderer>();
                    float alpha = Mathf.Abs(Mathf.Sin(Mathf.Deg2Rad * sp_opacity));
                    r.material.color = new Color(r.material.color.r, r.material.color.g, r.material.color.b, alpha);
                    sp_opacity = (sp_opacity + 4) % 360;
                    spawner.transform.position = hit.point;
                }

                if (Input.GetKeyDown("q") || OVRInput.GetDown(OVRInput.Button.One) || OVRInput.GetDown(OVRInput.Button.Three))
                {
                    GameObject laser_pointer = GameObject.Find("/OVREventSystem/LaserPointer");
                    GameObject new_browser = Instantiate(browser_prefab);
                    Camera camera = gameObject.GetComponent<Camera>();

                    new_browser.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_browser.transform.position = hit.point;

                    new_browser.transform.Find("Classes").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;
                    new_browser.transform.Find("Methods").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;
                    new_browser.transform.Find("CodeEditor").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;

                    new_browser.transform.Find("Classes").gameObject.GetComponent<Canvas>().worldCamera = camera;
                    new_browser.transform.Find("Methods").gameObject.GetComponent<Canvas>().worldCamera = camera;
                    new_browser.transform.Find("CodeEditor").gameObject.GetComponent<Canvas>().worldCamera = camera;
                }
                else if (Input.GetKeyDown("e") || OVRInput.GetDown(OVRInput.Button.Two) || OVRInput.GetDown(OVRInput.Button.Four))
                {
                    GameObject laser_pointer = GameObject.Find("/OVREventSystem/LaserPointer");
                    GameObject new_playground = Instantiate(playground_prefab);
                    Camera camera = gameObject.GetComponent<Camera>();

                    new_playground.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_playground.transform.position = hit.point;

                    new_playground.transform.Find("Editor").gameObject.GetComponent<OVRRaycaster>().pointer = laser_pointer;
                    new_playground.transform.Find("Editor").gameObject.GetComponent<Canvas>().worldCamera = camera;
                }
            }
        }

        if (Input.GetKeyUp(KeyCode.LeftControl) || 
            OVRInput.GetUp(OVRInput.Button.PrimaryIndexTrigger) ||
            OVRInput.GetUp(OVRInput.Button.SecondaryIndexTrigger))
            spawner.SetActive(false);

        else if (Input.GetKeyDown(KeyCode.LeftControl) ||
            OVRInput.GetDown(OVRInput.Button.PrimaryIndexTrigger) ||
            OVRInput.GetDown(OVRInput.Button.SecondaryIndexTrigger))
            spawner.SetActive(true);
    }
}
