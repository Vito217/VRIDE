using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class PlaygroundInit : MonoBehaviour
{
    public GameObject this_object;
    public GameObject this_panel;
    public float rise_speed = 1.0f;
    float y_limit = 4.0f;
    public bool initializing = true;
    Vector3 new_pos;

    // Start is called before the first frame update
    void Start()
    {
        this_panel.GetComponent<Image>().color = Random.ColorHSV();

        new_pos = new Vector3(this_object.transform.position.x,
                                   y_limit,
                                   this_object.transform.position.z);
    }

    // Update is called once per frame
    void Update()
    {
        if (initializing)
        {
            initializeAnimation();
        }
    }

    void initializeAnimation()
    {
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
            initializing = false;
        }
    }
}
