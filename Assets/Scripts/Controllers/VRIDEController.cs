using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;

public class VRIDEController : MonoBehaviour
{
    public GameObject browser_prefab;
    public GameObject playground_prefab;
    public GameObject spawner_prefab;
    public Camera camera;
    GameObject spawner;
    public float range = 100f;
    private int sp_opacity;

    private GameObject og_browser = null;

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
        if (Input.GetKey(KeyCode.LeftControl) || Input.GetKey(KeyCode.LeftCommand))
        {

            RaycastHit hit;

            if (Physics.Raycast(camera.transform.position, camera.transform.forward, out hit, range))
            {

                if (spawner.activeSelf)
                {
                    var r = spawner.GetComponent<Renderer>();
                    float alpha = Mathf.Abs(Mathf.Sin(Mathf.Deg2Rad * sp_opacity));
                    r.material.color = new Color(r.material.color.r, r.material.color.g, r.material.color.b, alpha);
                    sp_opacity = (sp_opacity + 4) % 360;
                    spawner.transform.position = hit.point;
                }

                if (Input.GetKeyDown("q"))
                {
                    GameObject new_browser;
                    if (og_browser == null)
                    {
                        new_browser = Instantiate(browser_prefab);
                        new_browser.transform.Find("Toolbar").gameObject.GetComponent<ToolbarBehaviour>().player = gameObject;
                        new_browser.transform.Find("CodeEditor/Panel/InputField (TMP)").gameObject.GetComponent<BrowserTextEditor>().player = gameObject;
                        og_browser = new_browser;
                    }
                    else
                    {
                        new_browser = Instantiate(og_browser);
                        new_browser.GetComponent<BrowserInit>().initializing = true;
                    }
                    new_browser.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_browser.transform.position = hit.point;
                }
                else if (Input.GetKeyDown("e"))
                {
                    GameObject new_playground = Instantiate(playground_prefab);
                    new_playground.transform.Find("Toolbar").gameObject.GetComponent<ToolbarBehaviour>().player = gameObject;
                    new_playground.transform.Find("Editor/Panel/InputField (TMP)").gameObject.GetComponent<PlaygroundTextEditor>().player = gameObject;
                    new_playground.transform.forward = new Vector3(transform.forward.x, 0, transform.forward.z);
                    new_playground.transform.position = hit.point;
                }
            }
        }

        if (Input.GetKeyUp(KeyCode.LeftControl) || Input.GetKeyUp(KeyCode.LeftCommand))
            spawner.SetActive(false);

        else if (Input.GetKeyDown(KeyCode.LeftControl) || Input.GetKeyDown(KeyCode.LeftCommand))
            spawner.SetActive(true);

        if (Input.GetKeyDown(KeyCode.Escape))
            Application.Quit();
    }
}
