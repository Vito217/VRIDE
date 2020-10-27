// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "ViveSR/depurpleShader"
{
    Properties
    {
        _MainTex("Texture", 2D) = "white" {}
        _gamma("gamma", Range(0.0,2.0)) = 0.8
        [Enum(Off, 0, On, 1)] _ZWrite("Z Write", int) = 1
        _StencilValue ("StencilRefValue", float) = 0
         [Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0 // disable
    }

    SubShader
    {
        // Make the render queue larger than 1000 and less than 2000.
        Tags { "RenderType"="Opaque" "Queue" = "Background+10" }
        ZTest Always
        ZWrite [_ZWrite]

        Stencil{
            Ref [_StencilValue]
            Comp[_StencilComp]
        }

        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "UnityCG.cginc"

            sampler2D _MainTex;
            float4 _MainTex_TexelSize;
            float _gamma;
            struct appdata
            {
                float4 pos : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 pos : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            v2f vert (appdata v)
            {
                v2f o;
                o.pos = UnityObjectToClipPos(v.pos);
                o.uv = v.uv;
                return o;
            }
            float rgb2y(float r0, float g0, float b0)
            {
                float y = 0.299f*r0 + 0.587f*g0 + 0.114f*b0 + 0.0f;
                return y;
            }
            float rgb2u(float r0, float g0, float b0)
            {
                float u = -0.169f*r0 - 0.331f*g0 + 0.5f*b0 + 0.50196;
                return u;
            }
            float rgb2v(float r0, float g0, float b0)
            {
                float v = 0.5f*r0 - 0.419f*g0 - 0.081f*b0 + 0.50196;
                return v;
            }
            float3 yuv2rgb(float y0, float u0, float v0)
            {
                u0 = u0 - 128;
                v0 = v0 - 128;
                float r0 = y0 - 0.00093*u0 + 1.401687*v0;
                float g0 = y0 - 0.3437*u0 - 0.71417*v0;
                float b0 = y0 + 1.77216*u0 + 0.00099*v0;
                return float3(r0, g0, b0);
            }
            float2 returnSrc(int i, int j)
            {
                float srcy = 0;
                float srcx = 0;
                int abs_i = abs(i);
                int abs_j = abs(j);
                int CLOSE = 1;
                int MEDIAN = 2;
                int FAR = 3;
                if (abs_i <= CLOSE && abs_j <= CLOSE)
                {
                    srcy = i * 3;
                    srcx = j * 3;
                }
                else if (abs_i <= MEDIAN && abs_j <= MEDIAN)
                {
                    srcy = i*1.5;
                    srcx = j*1.5;
                }
                else if (abs_i == FAR && abs_j == FAR)
                {
                    srcy = i*0.5 ;
                    srcx = j*0.5;
                }
                else if (abs_i == FAR)
                {
                    srcy = i*0.5;
                    srcx = j;
                }
                else if (abs_j == FAR)
                {
                    srcx = j*0.5;
                    srcy = i;
                }
                return float2(srcx, srcy);
            }
            float4 frag (v2f vIn) : SV_Target
            {
                fixed2 uv = vIn.uv;
                fixed3 vFragColour;

                fixed2 _MainTex_size = fixed2(_MainTex_TexelSize.x, _MainTex_TexelSize.y);

                fixed3 rgb = tex2D(_MainTex, uv);

                float y_map = rgb2y(rgb.r, rgb.g, rgb.b)*255.0;
                float u_map = rgb2u(rgb.r , rgb.g, rgb.b)*255.0;
                float v_map = rgb2v(rgb.r , rgb.g, rgb.b)*255.0;

                if (y_map < 200 && ( (u_map>120 && v_map>120) || (v_map>150) ) )
                {
                    for (int i = -3;i < 3;i++)
                    {
                        for (int j = -3;j < 3;j++)
                        {
                            float2 uv_light = uv + fixed2(j, i) * _MainTex_size;

                            float3 rgb_light = tex2D(_MainTex, uv_light);
                            float y_light = rgb2y(rgb_light.r, rgb_light.g, rgb_light.b)*255.0;
                            if (y_light< 200)
                                continue;
                            fixed2 src = returnSrc(i, j);
                            fixed2 uv_src = uv - src * _MainTex_size;

                            fixed3 neighbor = tex2D(_MainTex, uv_src);
                            float u_tmp = (rgb2u(neighbor.r, neighbor.g, neighbor.b)*255.0-128) *0.83333 +128;
                            float v_tmp = (rgb2v(neighbor.r, neighbor.g, neighbor.b)*255.0 - 128) *0.83333 + 128;
                            if ( v_map -v_tmp <= 5 )
                                continue;
                            u_map = u_tmp;
                            v_map = v_tmp;
                        }
                    }
                }
                float3 back2rgb = yuv2rgb(y_map, u_map, v_map);
                if (_gamma == 0.7|| _gamma == 1.1)
                {
                    back2rgb[0] = 255.0 * pow(back2rgb[0] * 0.0039216, _gamma);
                    back2rgb[1] = 255.0 * pow(back2rgb[1] * 0.0039216, _gamma);
                    back2rgb[2] = 255.0 * pow(back2rgb[2] * 0.0039216, _gamma);
                }

                for (int i = 0;i < 3;i++)
                {
                    back2rgb[i] = clamp(back2rgb[i], 0.0, 255.0);
                }
                vFragColour = fixed3(back2rgb[0] * 0.0039216, back2rgb[1] * 0.0039216, back2rgb[2] * 0.0039216);

                return fixed4(vFragColour, 1.0);
            }
            ENDCG
        }
    }
}
