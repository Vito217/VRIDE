using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Net.Http;
using UnityEngine.UI;
using TMPro;

public class SVGObjectInit : MonoBehaviour
{
    public bool initializing = false;
    public float expand_speed = 0.5f;
    public Vector3 new_pos;

    // Update is called once per frame
    void Update()
    {
        if (initializing)
            initializeAnimation();
    }

    void initializeAnimation()
    {
        //rise_speed = rise_speed * 0.92f;
        transform.position = Vector3.MoveTowards(
            transform.position,
            new_pos,
            expand_speed
        );

        if (transform.position == new_pos)
            initializing = false;
    }
}
