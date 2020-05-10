using System;
using System.Collections;
using System.Collections.Generic;
using System.Security.Cryptography;
using UnityEngine;

public class LightBehaviour : MonoBehaviour
{
    float grades;

    // Start is called before the first frame update
    void Start()
    {
        grades = 0.0f;
    }

    // Update is called once per frame
    void Update()
    {
        DateTime now = System.DateTime.Now;
        float seconds = now.Hour * 3600 + now.Minute * 60 + now.Second;
        if(0 <= grades && grades < 21600)
        {
            grades = (seconds / 240) + 270;
        }
        //else if(21600 <= grades && grades < 86400)
        else
        {
            grades = (seconds / 240) - 90;
        }
        transform.eulerAngles = new Vector3(grades,0,0);
    }
}
