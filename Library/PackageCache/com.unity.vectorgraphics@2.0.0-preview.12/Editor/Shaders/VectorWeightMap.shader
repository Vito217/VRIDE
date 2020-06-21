Shader "Unlit/VectorWeightMap"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
    }
    SubShader
    {
        Tags
        {
            "RenderType" = "Transparent"
            "Queue" = "Transparent"
            "IgnoreProjector" = "True"
            "PreviewType" = "Plane"
        }
        LOD 100

        Cull Off
        Lighting Off
        ZWrite Off
        Blend Off

        CGINCLUDE
        struct Input
        {
            float4 vertex : POSITION;
            float2 uv : TEXCOORD0;
            fixed4 color : COLOR;
        };

        struct Varying
        {
            float2 uv : TEXCOORD0;
            fixed4 color : COLOR;
            float4 vertex : SV_POSITION;
        };
        ENDCG

        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "UnityCG.cginc"
            
            Varying vert(Input i)
            {
                Varying o;
                o.vertex = UnityObjectToClipPos(i.vertex);
                o.color = i.color;
                return o;
            }

            fixed4 frag(Varying i) : SV_Target
            {
                return i.color;
            }
            ENDCG
        }

        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma target 3.0
            #include "UnityCG.cginc"

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

            Varying vert(Input i)
            {
                Varying output;
                output.vertex = UnityObjectToClipPos(i.vertex);
                output.uv = TRANSFORM_TEX(i.uv, _MainTex);
                return output;
            }

            half4 frag(Varying i) : SV_Target
            {
                float2 step = _MainTex_TexelSize.xy;
                fixed4 c = tex2D(_MainTex, i.uv);
                if (c.a == 1.0f)
                    return c;

                for (int tap = 0; tap < 8; ++tap)
                {
                    float2 uv = i.uv + kDirs[tap] * step;
                    fixed4 texCol = tex2D(_MainTex, uv);
                    if (texCol.a == 1.0f)
                    {
                        c = texCol;
                        break;
                    }
                }

                return c;
            }
            ENDCG
        }
    }
}
