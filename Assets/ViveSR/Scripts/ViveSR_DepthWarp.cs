//The region of Deprecation period API will remove in the future.
#region Deprecation period API
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    /**
    * @warning The class will remove in the future.
    */
    [RequireComponent(typeof(Material))]
    public class ViveSR_DepthWarp : MonoBehaviour
    {
        public ViveSR_DepthWarpToRightEye new_interface_depth_warp;
        public ComputeShader _compute_shader;
        public Material _render_mat;
        public ComputeShader _computeShader
        {
            get { return _compute_shader; }
            set { _compute_shader = value; }
        }
        public Material _renderMat
        {
            get { return _render_mat; }
            set { _render_mat = value; }
        }
        private int _kernel;
        private RenderTexture _warp_depth;
        private Material _clear_mat;

        private Vector4 _depth_param = new Vector4();        // focalL, baseline, minDepth, maxDepth
        private int _width;
        private int _height;
        private double focal_length = 0;
        private double base_line = 0;
        void Start()
        {
            new_interface_depth_warp = new ViveSR_DepthWarpToRightEye();
            if (_render_mat != null)
                new_interface_depth_warp._renderMat = _renderMat;
            if (_compute_shader != null)
            {
                new_interface_depth_warp._compute_shader = _compute_shader;
                _clear_mat = new Material(Shader.Find("Unlit/Color"));
                _clear_mat.color = Color.black;

                // kernel
                _kernel = _compute_shader.FindKernel("CSMain");

                // constant buffer
                _width = ViveSR_DualCameraImageCapture.DepthImageWidth;
                _height = ViveSR_DualCameraImageCapture.DepthImageHeight;
                SRWorkModule_API.GetDepthParameterDouble((int)DepthParam.FOCULENS, ref focal_length);
                SRWorkModule_API.GetDepthParameterDouble((int)DepthParam.BASELINE, ref base_line);
                _depth_param.x = (float)focal_length;
                _depth_param.y = (float)base_line;
                _depth_param.z = ViveSR_DualCameraImageRenderer.OcclusionNearDistance;
                _depth_param.w = ViveSR_DualCameraImageRenderer.OcclusionFarDistance;

                // input texture
                int frame_index, time_index;
                Texture2D texture_depth;
                Matrix4x4 pose_left;
                ViveSR_DualCameraImageCapture.GetDepthTexture(out texture_depth, out frame_index, out time_index, out pose_left);

                // result texture
                _warp_depth = new RenderTexture(_width, _height, 0, RenderTextureFormat.RFloat);
                _warp_depth.enableRandomWrite = true;
                _warp_depth.Create();

                // bind
                _compute_shader.SetInt("ImageWidth", _width);
                _compute_shader.SetVector("DepthParam", _depth_param);
                _compute_shader.SetTexture(_kernel, "DepthInput", texture_depth);
                _compute_shader.SetTexture(_kernel, "Result", _warp_depth);
            }
        }

        void OnApplicationQuit()
        {
            if (_warp_depth != null)
                _warp_depth.Release();
        }

        // Update is called once per frame
        void Update()
        {
            if (_compute_shader != null && _render_mat != null)
            {
                _RunShader();
                _render_mat.mainTexture = _warp_depth;
            }
        }

        void _RunShader()
        {
            // Clear RT        
            Graphics.Blit(null, _warp_depth, _clear_mat);

            // Warp
            if (_depth_param.z != ViveSR_DualCameraImageRenderer.OcclusionNearDistance || _depth_param.w != ViveSR_DualCameraImageRenderer.OcclusionFarDistance)
            {
                _depth_param.z = ViveSR_DualCameraImageRenderer.OcclusionNearDistance;
                _depth_param.w = ViveSR_DualCameraImageRenderer.OcclusionFarDistance;
                _compute_shader.SetVector("DepthParam", _depth_param);
            }

            _compute_shader.Dispatch(_kernel, _width / 8, _height / 8, 1);
        }
    }
}
#endregion