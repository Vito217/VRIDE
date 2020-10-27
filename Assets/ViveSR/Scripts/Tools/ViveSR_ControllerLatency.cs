using UnityEngine;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Vive.Plugin.SR
{
    public class ViveSR_ControllerLatency : MonoBehaviour
    {
        private TrackingPoseInfo poseInfo;
        public enum ControllerHandType
        {
            NoHand,
            LeftHand,
            RightHand,
        }
        protected struct PoseWithTimestamp
        {
            public float timestamp;
            public Vector3 position;
            public Quaternion rotation;
        }

        protected struct ObjectToMoveData
        {
            public Transform trans;
            public Vector3 pos;
            public Vector3 rot;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct TrackingPoseInfo
        {
            public System.Int32 PoseIsValid_Left;
            public System.Int32 PoseIsValid_Right;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)]
            public float[] Position_Left;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)]
            public float[] Position_Right;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
            public float[] Rotation_Left;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
            public float[] Rotation_Right;
        };

        [Tooltip("Just move the objct that ControllerLatency attached to")]
        public bool moveAttachedObject = true;

        [Tooltip("Tracking left or right hand controller.")]
        public ControllerHandType trackController = ControllerHandType.NoHand;

        [Tooltip("Latency in milliseconds to be applied on the movement of controller to fit real world controller in PassThrough image.")]
        [Range(0, 200)]
        public int latencyCompensation = 60;

        protected List<PoseWithTimestamp> latencyMap = new List<PoseWithTimestamp>();

        protected List<ObjectToMoveData> objectToMove = new List<ObjectToMoveData>();

        public void AddObjectToMove(Transform trans, Vector3 pos, Vector3 rot)
        {
            ObjectToMoveData data;
            data.trans = trans;
            data.pos = pos;
            data.rot = rot;

            objectToMove.Add(data);
        }

        public static int GetControllerTrackingPose(ref TrackingPoseInfo poseInfo)
        {
            int result = SRWorkModule_API.GetControllerTrackingPose(ref poseInfo);
            return result;
        }

        private void UpdateControllerLocation()
        {
            bool result = ControllPose.SRWork_Controll_Pose.UpdateData();

            if (result)
            {
                poseInfo.PoseIsValid_Left = (System.Int32)ControllPose.SRWork_Controll_Pose.controller_pose_data_.pose_is_valid_left;
                poseInfo.PoseIsValid_Right = (System.Int32)ControllPose.SRWork_Controll_Pose.controller_pose_data_.pose_is_valid_right;
                poseInfo.Position_Left = ControllPose.SRWork_Controll_Pose.pos_left;
                poseInfo.Rotation_Left = ControllPose.SRWork_Controll_Pose.rot_left;
                poseInfo.Position_Right = ControllPose.SRWork_Controll_Pose.pos_right;
                poseInfo.Rotation_Right = ControllPose.SRWork_Controll_Pose.rot_right;

                if (trackController == ControllerHandType.LeftHand)
                {
                    if (poseInfo.PoseIsValid_Left != 0)
                        AddPoseToMap(new Vector3(poseInfo.Position_Left[0], poseInfo.Position_Left[1], poseInfo.Position_Left[2]),
                                     new Quaternion(poseInfo.Rotation_Left[0], poseInfo.Rotation_Left[1], poseInfo.Rotation_Left[2], poseInfo.Rotation_Left[3]));
                }
                else if (trackController == ControllerHandType.RightHand)
                {
                    if (poseInfo.PoseIsValid_Right != 0)
                        AddPoseToMap(new Vector3(poseInfo.Position_Right[0], poseInfo.Position_Right[1], poseInfo.Position_Right[2]),
                                     new Quaternion(poseInfo.Rotation_Right[0], poseInfo.Rotation_Right[1], poseInfo.Rotation_Right[2], poseInfo.Rotation_Right[3]));
                }
            }

            if (latencyCompensation > 0 && latencyMap.Count > 0)
            {                
                PoseWithTimestamp pose = GetLatencyPosition((float)(latencyCompensation / 1000.0f));

                if (moveAttachedObject)
                {
                    transform.position = pose.position;
                    transform.rotation = pose.rotation;
                }
                else if (objectToMove.Count > 0)
                {
                    for (int i = 0; i < objectToMove.Count; ++i)
                    {
                        objectToMove[i].trans.position = pose.position;
                        objectToMove[i].trans.rotation = pose.rotation;

                        objectToMove[i].trans.localPosition += objectToMove[i].pos;
                        objectToMove[i].trans.rotation *= Quaternion.Euler(objectToMove[i].rot);
                    }
                }
            }
        }

        private PoseWithTimestamp GetLatencyPosition(float timeDelay)
        {
            PoseWithTimestamp pose = new PoseWithTimestamp();

            //Use latest first
            pose.position = latencyMap[latencyMap.Count - 1].position;
            pose.rotation = latencyMap[latencyMap.Count - 1].rotation;

            for (int i = 0; i < latencyMap.Count; ++i)
            {
                // Find first one that newer than target time
                if (latencyMap[i].timestamp > Mathf.Abs(Time.time - timeDelay))
                {
                    if (i > 0)
                    {
                        //Linearly interpolates between two poses
                        float delta = latencyMap[i].timestamp - latencyMap[i - 1].timestamp;
                        float alpha = ((Time.time - latencyMap[i - 1].timestamp) - timeDelay) / delta;
                        
                        Vector3 pos = Vector3.Lerp(latencyMap[i - 1].position, latencyMap[i].position, alpha);
                        Quaternion rot = Quaternion.Lerp(latencyMap[i - 1].rotation, latencyMap[i].rotation, alpha);

                        pose.position = pos;
                        pose.rotation = rot;
                        
                        latencyMap.RemoveRange(0, i - 1);
                    }
                    return pose;
                }
            }
            return pose;
        }

        // Add pose with current timestamp
        private void AddPoseToMap(Vector3 position, Quaternion rot)
        {
            PoseWithTimestamp currentPoseData = new PoseWithTimestamp();
            currentPoseData.timestamp = Time.time;
            currentPoseData.rotation = rot;
            currentPoseData.position = position;
            
            latencyMap.Add(currentPoseData);
        }

        private void Awake()
        {
            latencyMap.Clear();
            objectToMove.Clear();
        }

        private void Start()
        {
            poseInfo = new TrackingPoseInfo();
            poseInfo.Position_Left = new float[3];
            poseInfo.Rotation_Left = new float[4];
            poseInfo.Position_Right = new float[3];
            poseInfo.Rotation_Right = new float[4];
        }

        private void Update()
        {
            if (trackController != ControllerHandType.NoHand)
                UpdateControllerLocation();
        }
    }
}

