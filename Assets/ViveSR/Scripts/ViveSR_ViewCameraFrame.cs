using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public class ViveSR_ViewCameraFrame : MonoBehaviour
    {
        private void Start()
        {
            Matrix4x4 CamMat = gameObject.transform.parent.GetComponent<Camera>().projectionMatrix;
            float XScale = 1.0f / CamMat[0, 0];
            float YScale = 1.0f / CamMat[1, 1];

            Mesh MyMesh = GetComponent<MeshFilter>().mesh;
            Vector3[] Pos = MyMesh.vertices;

            for (int i = 0; i < Pos.Length; i++)
            {
                Vector3 Temp = Pos[i];
                Temp.x *= 2 * XScale * 1.2f; // 2 * scale is fitting camera view, but we need larger for camera stereo separation.
                Temp.y *= 2 * YScale * 1.2f; // 2 * scale is fitting camera view, but we need larger for camera stereo separation.
                Pos[i] = Temp;
            }
            MyMesh.vertices = Pos;
        }
        public void SetFrame(float WidthBlockRate,float HeightBlockRate)
        {
            WidthBlockRate *= 1.2f; // we enlarge 20% for camera stereo separation, recover it.
            HeightBlockRate *= 1.2f; // we enlarge 20% for camera stereo separation, recover it.
            const int Border = 20;
            const int Height = 100;
            const int Width = 100;
            Texture2D Tex= new Texture2D(Width + Border, Height + Border, TextureFormat.RGBA32, false); // 0~9 and 110~119 is always black, user can control 10~109 only.
            Color[] Texels = Tex.GetPixels();
            int Xbond = Border / 2 + (int)((float)Width * (1.0f - WidthBlockRate) / 2.0f);
            int Ybond = Border / 2 + (int)((float)Height * (1.0f - HeightBlockRate) / 2.0f);
            for (int i=0;i< Texels.Length;i++)
            {
                int x = i % (Width + Border);
                int y = i / (Width + Border);
                if(x >Xbond && x < (Width + Border) - Xbond && y >Ybond && y < (Height + Border) -Ybond)
                {
                    Texels[i] = Color.clear;
                }
                else
                {
                    Texels[i] = Color.black;
                }
            }
            Tex.SetPixels(Texels);
            Tex.Apply();

            GetComponent<Renderer>().material.mainTexture = Tex;
        }
    }
}
