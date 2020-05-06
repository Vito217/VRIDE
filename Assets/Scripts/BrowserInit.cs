using System.Collections;
using System.Collections.Generic;
using UnityEngine.UI;
using UnityEngine;

public class BrowserInit : MonoBehaviour
{
    public GameObject classes_window;
    public GameObject methods_window;
    public GameObject text_editor;
    public GameObject this_object;
    public GameObject classes_panel;
    public GameObject methods_panel;
    public GameObject editor_panel;
    public float rise_speed = 1.0f;
    public float expand_speed = 1.0f;
    Vector3 new_classes_pos;
    Vector3 new_texteditor_pos;
    Vector3 new_pos;
    public float y_limit = 4.0f;
    public bool initializing = true;

    // Start is called before the first frame update
    void Start()
    {
        Color new_color = Random.ColorHSV();
        classes_panel.GetComponent<Image>().color = new_color;
        methods_panel.GetComponent<Image>().color = new_color;
        editor_panel.GetComponent<Image>().color = new_color;

        new_pos = new Vector3(this_object.transform.position.x, 
                                      y_limit,
                                      this_object.transform.position.z);
    }

    // Update is called once per frame
    void Update()
    {
        if (initializing)
            initializeAnimation();        
    }

    void initializeAnimation() {
        if (this_object.transform.position.y < y_limit)
        {
            //rise_speed = rise_speed * 0.92f;
            this_object.transform.position = Vector3.MoveTowards(
                this_object.transform.position,
                new_pos,
                rise_speed
            );
        }
        else
            initializing = false;
    }
}
