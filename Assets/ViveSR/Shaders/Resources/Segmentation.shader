Shader "ViveSR/Segmentation"
{
    Properties
    {
        _MainTex("Base Texture", 2D) = "white" {}
        _MaskTex("MaskTexture", 2D) = "white" {}
        [Enum(Off, 0, On, 1)] _ZWrite("Z Write", int) = 1										// default
        _StencilValue("StencilRefValue", float) = 0
        [Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
        _SegmentWay("SegmentWay", Range(0,2)) = 0
        _Image4kReady("Image4kReady", Range(0,1)) = 0
        _HueMin("HueMin", Range(0.0,1.0)) = 0.3
        _HueMax("HueMax", Range(0.0,1.0)) = 0.5
        _SaturationMin("SaturationMin", Range(0.0,1.0)) = 0.3783
        _SaturationMax("SaturationMax", Range(0.0,1.0)) = 1.0
        _ValueMin("ValueMin", Range(0.0,1.0)) = 0.1995
        _ValueMax("ValueMax", Range(0.0,1.0)) = 1.0

    }

    SubShader
    {
        Tags{ "Queue" = "Transparent" "IgnoreProjector" = "True" "RenderType" = "Transparent" }
        Blend SrcAlpha OneMinusSrcAlpha
        ZTest Always
        ZWrite[_ZWrite]

        Stencil{
        Ref[_StencilValue]
        Comp[_StencilComp]
        }

        Pass
        {
            CGPROGRAM
            #pragma multi_compile __ SRWORKS_GAMMA_TO_LINEAR
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "UnityCG.cginc"
            int _SegmentWay;
            int _Image4kReady;
            float _HueMin;
            float _HueMax;
            float _SaturationMin;
            float _SaturationMax;
            float _ValueMin;
            float _ValueMax;
            float _MinDistance;
            float _MaxDistance;
            sampler2D _MainTex;
            float4 _MainTex_TexelSize;
            sampler2D _MaskTex;
            float4 _MaskTex_TexelSize;
            float _Scale;
            struct vInput
            {
                float4 pos : POSITION;
                float2 uvCoord : TEXCOORD0;
            };

            struct fInput
            {
                float4 pos : SV_POSITION;
                float2 uvCoord : TEXCOORD0;
                UNITY_FOG_COORDS(1)
            };

            fInput vert(vInput vIn)
            {
                fInput vOut;

                vOut.pos = UnityObjectToClipPos(vIn.pos);
                vOut.uvCoord = vIn.uvCoord;
                UNITY_TRANSFER_FOG(vOut, vOut.pos);
                return vOut;
            }

            float3 RGB2HSV(float4 c)
            {
                float4 K = float4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
                float4 p = lerp(float4(c.bg, K.wz), float4(c.gb, K.xy), step(c.b, c.g));
                float4 q = lerp(float4(p.xyw, c.r), float4(c.r, p.yzx), step(p.x, c.r));

                float d = q.x - min(q.w, q.y);
                float e = 1.0e-10;
                return float3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
            }

            float4 frag(fInput fIn) : SV_Target
            {
                fixed4 col = tex2D(_MainTex,fIn.uvCoord);
                switch (_SegmentWay)
                {
                case 0:
                {
                    if (_Image4kReady == 1) {
                        fixed4 label = tex2D(_MaskTex, fIn.uvCoord);
                        if (label.r > 0.0f)
                            col.a = 0.0f;
                    }
                    else {
                        int y = fIn.uvCoord.y * _MainTex_TexelSize.w; //750
                        int x = fIn.uvCoord.x * _MainTex_TexelSize.z; //1150
                        col.a = 0.0f;
                        fixed2 _MaskTex_size = fixed2(_MaskTex_TexelSize.x, _MaskTex_TexelSize.y);
                        int left_top_x = (_MainTex_TexelSize.z - _MaskTex_TexelSize.z)*0.5;
                        int left_top_y = (_MainTex_TexelSize.w - _MaskTex_TexelSize.w)*0.5;
                        if (x >= left_top_x && x < (left_top_x + _MaskTex_TexelSize.z)  &&
                            y >= left_top_y && y < (left_top_y + _MaskTex_TexelSize.w)) {
                            col.a = 1.0f;
                            fixed2 new_xy = fixed2(x - left_top_x, y - left_top_y);
                            fixed4 label = tex2D(_MaskTex, new_xy*_MaskTex_size);
                            if (label.r > 0.0f)
                                col.a = 0.0f;
                        }
                    }
                    break;
                }
                case 1:
                {
                    if (_Image4kReady == 1) {
                        col.a = 0.0f;
                        fixed2 _MaskTex_size = fixed2(_MaskTex_TexelSize.x, _MaskTex_TexelSize.y);

                        float y = fIn.uvCoord.y * _MainTex_TexelSize.w; //2424
                        float x = fIn.uvCoord.x * _MainTex_TexelSize.z; //2424
                        float scale_x = x / _Scale;
                        float scale_y = y / _Scale;
                        float scale_width = (float)_MainTex_TexelSize.z / _Scale;
                        float scale_height = (float)_MainTex_TexelSize.w / _Scale;
                        int left_top_x = (scale_width - _MaskTex_TexelSize.z)*0.5;
                        int left_top_y = (scale_height - _MaskTex_TexelSize.w)*0.5;
                        if (scale_x >= left_top_x && scale_x < (left_top_x + _MaskTex_TexelSize.z) &&
                            scale_y >= left_top_y && scale_y < (left_top_y + _MaskTex_TexelSize.w)) {
                            col.a = 1.0f;
                            fixed2 new_xy = fixed2(scale_x - left_top_x, scale_y - left_top_y);
                            if (new_xy.x >= 0 && new_xy.x < _MaskTex_TexelSize.z && new_xy.y >= 0 && new_xy.y < _MaskTex_TexelSize.w) {
                                fixed4 label = tex2D(_MaskTex, new_xy*_MaskTex_size);
                                if (label.r <_MinDistance || label.r>_MaxDistance)
                                    col.a = 0.0f;
                            }
                        }
                    }
                    else {
                        int y = fIn.uvCoord.y * _MainTex_TexelSize.w; //750
                        int x = fIn.uvCoord.x * _MainTex_TexelSize.z; //1150

                        col.a = 0.0f;
                        fixed2 _MaskTex_size = fixed2(_MaskTex_TexelSize.x, _MaskTex_TexelSize.y);
                        int left_top_x = (_MainTex_TexelSize.z - _MaskTex_TexelSize.z)*0.5;
                        int left_top_y = (_MainTex_TexelSize.w - _MaskTex_TexelSize.w)*0.5;
                        if (x >= left_top_x && x < (left_top_x + _MaskTex_TexelSize.z) &&
                            y >= left_top_y && y < (left_top_y + _MaskTex_TexelSize.w)) {
                            col.a = 1.0f;
                            fixed2 new_xy = fixed2(x - left_top_x, y - left_top_y);
                            if (new_xy.x >= 0 && new_xy.x < _MaskTex_TexelSize.z && new_xy.y >= 0 && new_xy.y < _MaskTex_TexelSize.w) {
                                fixed4 label = tex2D(_MaskTex, new_xy*_MaskTex_size);
                                if (label.r <_MinDistance || label.r>_MaxDistance)
                                    col.a = 0.0f;
                            }
                        }
                    }
                    break;
                }
                case 2:
                {
                    col.a = 1.0f;
                    float3 hsv = RGB2HSV(col);
                    if(hsv.r>= _HueMin && hsv.r<= _HueMax &&
                        hsv.g>=_SaturationMin && hsv.g<=_SaturationMax &&
                        hsv.b>= _ValueMin && hsv.b <= _ValueMax)
                    {
                        col.a = 0.0f;
                    }
                    else
                    {
                        bool self_transparent = false;
                        static const float2 vec[4] = {
                            float2(1.0f, 1.0f),
                            float2(1.0f, -1.0f),
                            float2(-1.0f, 1.0f),
                            float2(-1.0f,-1.0f)
                        };
                        fixed2 _MainTex_size = fixed2(_MainTex_TexelSize.x, _MainTex_TexelSize.y);
                        for (int i = 0;i < 4;i++)
                        {
                            fixed4 label = tex2D(_MainTex, fIn.uvCoord + vec[i] * _MainTex_size);
                            float3 hsv = RGB2HSV(label);
                            if (hsv.r >= _HueMin && hsv.r <= _HueMax &&
                                hsv.g >= _SaturationMin && hsv.g <= _SaturationMax &&
                                hsv.b >= _ValueMin && hsv.b <= _ValueMax)
                            {
                                col.a = 0.0f;
                                break;
                            }
                        }
                    }
                    break;
                }
                default:
                    break;
                }
                UNITY_APPLY_FOG(fIn.fogCoord, col);
                return col;
            }
            ENDCG
        }
    }
}
