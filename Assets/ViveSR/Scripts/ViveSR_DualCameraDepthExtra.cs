using System;
using System.Runtime.InteropServices;
using Vive.Plugin.SR;

namespace Vive.Plugin.SR
{
    public class ViveSR_DualCameraDepthExtra
    {        
        private static int[] RawDepthColliderTimeIndex = new int[1];
        private static int[] RawDepthColliderVerticesNum = new int[1];
        private static int[] RawDepthColliderBytePervert = new int[1];
        private static int[] RawDepthColliderIndicesNum = new int[1];
        private static float[] RawDepthColliderVertices;
        private static int[] RawDepthColliderIndices;

        private static int LastDepthColliderTimeIndex = -1;
        public static int DepthColliderTimeIndex { get { return RawDepthColliderTimeIndex[0]; } }
        public static int ColliderVerticeNum { get { return RawDepthColliderVerticesNum[0]; } }
        public static int ColliderBytePervert { get { return RawDepthColliderBytePervert[0]; } }
        public static int ColliderIndicesNum { get { return RawDepthColliderIndicesNum[0]; } }


        public static void InitialDepthCollider(int depthImageWidth, int depthImageHeight)
        {
            RawDepthColliderVertices = new float[depthImageWidth * depthImageHeight * 3];
            RawDepthColliderIndices = new int[depthImageWidth * depthImageHeight * 6];
        }
        public static void ReleaseDepthCollider()
        {
            RawDepthColliderVertices = null;
            RawDepthColliderIndices = null;
        }
        public static bool GetDepthColliderFrameInfo()
        {
            RawDepthColliderTimeIndex[0] = DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.time_stp;
            if (LastDepthColliderTimeIndex == DepthColliderTimeIndex) return false;
            else LastDepthColliderTimeIndex = DepthColliderTimeIndex;
            return true;
        }

        public static bool GetDepthColliderData(ref int verticesNum, out float[] verticesBuff, ref int indicesNum, out int[] indicesBuff)
        {

            RawDepthColliderVerticesNum[0] = DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.num_vertices;
            RawDepthColliderBytePervert[0] = DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.bytepervert;
            Marshal.Copy(DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.vertices, RawDepthColliderVertices, 0, ColliderVerticeNum * ColliderBytePervert / 3);
            RawDepthColliderIndicesNum[0] = DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.num_indices;
            Marshal.Copy(DepthMesh.SRWork_Depth_Mesh.depth_mesh_data_.indices, RawDepthColliderIndices, 0, ColliderIndicesNum);

            verticesNum = ColliderVerticeNum;
            indicesNum = ColliderIndicesNum;
            verticesBuff = RawDepthColliderVertices;
            indicesBuff = RawDepthColliderIndices;
            return true;
        }
    }
}