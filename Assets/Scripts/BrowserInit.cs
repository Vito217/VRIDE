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
    float y_limit = 4.0f;
    public bool initializing = true;

    // Start is called before the first frame update
    void Start()
    {
        Color new_color = Random.ColorHSV();
        classes_panel.GetComponent<Image>().color = new_color;
        methods_panel.GetComponent<Image>().color = new_color;
        editor_panel.GetComponent<Image>().color = new_color;

        Vector3 methods_pos = methods_window.transform.localPosition;
        classes_window.transform.localPosition = methods_pos;
        text_editor.transform.localPosition = methods_pos;

        float width = methods_window.GetComponent<RectTransform>().sizeDelta.x;

        new_classes_pos = new Vector3(methods_window.transform.localPosition.x - width - 1f,
                                      methods_window.transform.localPosition.y,
                                      methods_window.transform.localPosition.z);

        new_texteditor_pos = new Vector3(methods_window.transform.localPosition.x + width + 1f + 0.5f * width,
                                         methods_window.transform.localPosition.y,
                                         methods_window.transform.localPosition.z);

        new_pos = new Vector3(this_object.transform.position.x, 
                                      y_limit,
                                      this_object.transform.position.z);
    }

    // Update is called once per frame
    void Update()
    {
        if (initializing) {
            initializeAnimation();
        }
        
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
        {
            //expand_speed = expand_speed * 0.9f;
            classes_window.transform.localPosition = Vector3.MoveTowards(
                classes_window.transform.localPosition,
                new_classes_pos,
                expand_speed
            );

            text_editor.transform.localPosition = Vector3.MoveTowards(
                text_editor.transform.localPosition,
                new_texteditor_pos,
                expand_speed
            );

            if (text_editor.transform.localPosition.x >= new_texteditor_pos.x) {
                initializing = false;
            }
        }
    }
}
