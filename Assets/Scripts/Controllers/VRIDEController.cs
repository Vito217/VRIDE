using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using UnityEngine;
using Valve.VR;
using Valve.VR.InteractionSystem;

public class VRIDEController : MonoBehaviour
{
    public InitializeBehaviour browser_prefab;
    public InitializeBehaviour playground_prefab;
    public GameObject spawner_prefab;
    public Camera camera;
    GameObject spawner;
    public float range = 100f;
    private int sp_opacity;
    public bool can_move = true;
    private InitializeBehaviour og_browser = null;

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
                if (Input.GetKeyDown("q") || Input.GetKeyDown("e"))
                {
                    InitializeBehaviour new_window;
                    if (Input.GetKeyDown("q")){
                        if (og_browser == null)
                        {
                            new_window = Instantiate(browser_prefab);
                            og_browser = new_window;
                        }
                        else
                            new_window = Instantiate(og_browser);
                    }
                    else
                        new_window = Instantiate(playground_prefab);
                    new_window.Initialize(
                        hit.point,
                        new Vector3(hit.point.x, 2f, hit.point.z),
                        new Vector3(transform.forward.x, 0, transform.forward.z),
                        gameObject
                    );
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
