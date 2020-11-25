using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace UnityEngine.XR.WindowsMR.Tests
{
    public class FpsMeasure : MonoBehaviour
    {
        public int m_NonPerformantFrameCount;

        // we have observed a drop in performance between simulation and runtime
        // on the device - in the editor, we've seen it fluctuate from 45-100 FPS
        // when the device runs just fine (also giving a little bit of elbow room
        // for when editor tanks the frame rate a bit more than what we've seen)
        const float k_FrameTimeMax = 1f / 82f;

        public void Update()
        {
            //Current Bug where start up is causing fps slow down due to spin up [1115410]
            if (Time.deltaTime > k_FrameTimeMax)
                ++m_NonPerformantFrameCount;
        }
    }

}
