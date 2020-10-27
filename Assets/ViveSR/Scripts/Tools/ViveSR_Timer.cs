//========= Copyright 2017, HTC Corporation. All rights reserved. ===========

using System;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_Timer
    {
        private DateTime StartTime;
        private List<float> RecordTimeDatas = new List<float>();

        public void Start()
        {
            StartTime = DateTime.Now;
        }

        public float End()
        {
            float Time = (DateTime.Now - StartTime).Milliseconds;
            RecordTimeDatas.Add(Time);
            return Time;
        }

        public void Add(float time)
        {
            RecordTimeDatas.Add(time);
        }

        public float AverageAll()
        {
            float Sum = 0;
            for (int i = 0; i < RecordTimeDatas.Count; i++)
            {
                Sum += RecordTimeDatas[i];
            }
            return Sum / RecordTimeDatas.Count;
        }

        public float AverageWithout(int count)
        {
            if (count >= RecordTimeDatas.Count) return -Mathf.Infinity;
            float Sum = 0;
            for (int i = count; i < RecordTimeDatas.Count; i++)
            {
                Sum += RecordTimeDatas[i];
            }
            return Sum / (RecordTimeDatas.Count - count);
        }

        public float AverageLeast(int count)
        {
            if (count <= 0 || count > RecordTimeDatas.Count) return -Mathf.Infinity;
            float Sum = 0;
            for (int i = RecordTimeDatas.Count - count; i < RecordTimeDatas.Count; i++)
            {
                Sum += RecordTimeDatas[i];
            }
            return Sum / count;
        }
    }
}