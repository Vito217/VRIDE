Shader "Hidden/VectorExpandEdges"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
    }
    SubShader
    {
        Tags
        {
            "IgnoreProjector" = "True"
            "PreviewType" = "Plane"
        }
        LOD 100

        Cull Off
        Lighting Off
        ZWrite Off
        Blend One OneMinusSrcAlpha

        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            sampler2D _MainTex;
            float4 _MainTex_ST;
            float4 _MainTex_TexelSize;
            
            static const half2 kDirs[8] = {
                half2(-1, -1),
                half2( 1, -1),
                half2(-1,  1),
                half2( 1,  1),
                half2( 0, -1),
                half2(-1,  0),
                half2( 1,  0),
                half2( 0,  1)
            };

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                return o;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                float2 step = _MainTex_TexelSize.xy;
                fixed4 col = tex2D(_MainTex, i.uv);
                if (col.r != 0.0f || col.g != 0.0f || col.b != 0.0f || col.a != 0.0f)
                    return fixed4(col.rgb, 0.0f);

                fixed4 c = col;
                for (int tap = 0; tap < 8; ++tap)
                {
                    float2 uv = i.uv + kDirs[tap] * step;
                    fixed4 texCol = tex2D(_MainTex, uv);
                    if (texCol.a > 0.0f)
                    {
                        c = texCol;
                        break;
                    }
                }

                c.a = 0.0f;
                return c;
            }
            ENDCG
        }
    }
}
