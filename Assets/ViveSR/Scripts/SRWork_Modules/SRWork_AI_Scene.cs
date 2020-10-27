using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;
namespace Vive
{
    namespace Plugin.SR
    {
        namespace AIScene
        {
            public class SRWork_AI_Scene
            {
                public static int FrameImageWidth = 640, FrameImageHeight = 480, FrameImageChannel = 4;
                public static int LabelImageWidth = 640, LabelImageHeight = 480, LabelImageChannel = 1;
                private static int LastUpdateFrame = -1;
                private static int LastUpdateResult = (int)Error.FAILED;
                public static AISceneData ai_scene_data_;
                static SRWork_AI_Scene()
                {
                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Img_Crop_W, ref FrameImageWidth);
                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Img_Crop_H, ref FrameImageHeight);
                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Img_CH, ref FrameImageChannel);

                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Label_W, ref LabelImageWidth);
                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Label_H, ref LabelImageHeight);
                    SRWorkModule_API.GetAISceneParameterInt((int)AISceneParam.Label_CH, ref LabelImageChannel);

                    ai_scene_data_.frame_left = Marshal.AllocCoTaskMem(sizeof(char) * FrameImageWidth * FrameImageHeight * FrameImageChannel);
                    ai_scene_data_.label_left = Marshal.AllocCoTaskMem(sizeof(char) * LabelImageWidth * LabelImageHeight * LabelImageChannel);
                    ai_scene_data_.frame_right = Marshal.AllocCoTaskMem(sizeof(char) * FrameImageWidth * FrameImageHeight * FrameImageChannel);
                    ai_scene_data_.label_right = Marshal.AllocCoTaskMem(sizeof(char) * LabelImageWidth * LabelImageHeight * LabelImageChannel);

                    ai_scene_data_.pose_left = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                    ai_scene_data_.pose_right = Marshal.AllocCoTaskMem(sizeof(float) * 16);
                }
                public static bool UpdateData()
                {
                    if (Time.frameCount == LastUpdateFrame)
                        return LastUpdateResult == (int)Error.WORK;
                    else
                        LastUpdateFrame = Time.frameCount;

                    LastUpdateResult = SRWorkModule_API.GetAISceneData(ref ai_scene_data_);
                    return LastUpdateResult == (int)Error.WORK;
                }
            }
        }
    }
}
