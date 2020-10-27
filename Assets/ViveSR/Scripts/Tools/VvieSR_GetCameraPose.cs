using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VvieSR_GetCameraPose : MonoBehaviour {
    private GameObject render_pose;
    public float offset_x;
    public float offset_y;
    private static Vector3 camera_pose;
	// Update is called once per frame
	void Update () {
        camera_pose = Vive.Plugin.SR.ViveSR_DualCameraRig.Instance.DualCameraLeft.transform.position;
        transform.position = new Vector3(camera_pose.x + offset_x, camera_pose.y + offset_y, camera_pose.z + 2.0f);
    }
}
