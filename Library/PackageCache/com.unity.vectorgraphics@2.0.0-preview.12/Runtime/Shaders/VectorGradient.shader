Shader "Unlit/VectorGradient"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
        _Color ("Tint", Color) = (1,1,1,1)
        [HideInInspector] _RendererColor ("RendererColor", Color) = (1,1,1,1)
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
        Blend One OneMinusSrcAlpha

        Pass
        {
            CGPROGRAM
            #pragma vertex GradientVert
            #pragma fragment GradientFrag
            #pragma multi_compile_instancing

            #include "UnityCG.cginc"
            #include "VectorGradient.cginc"

            #ifdef UNITY_INSTANCING_ENABLED
            UNITY_INSTANCING_BUFFER_START(PerDrawSprite)
                UNITY_DEFINE_INSTANCED_PROP(fixed4, unity_SpriteRendererColorArray)
            UNITY_INSTANCING_BUFFER_END(PerDrawSprite)
            #define _RendererColor  UNITY_ACCESS_INSTANCED_PROP(PerDrawSprite, unity_SpriteRendererColorArray)
            #endif

            #ifndef UNITY_INSTANCING_ENABLED
            fixed4 _RendererColor;
            #endif

            struct appdata
            {
                float4 vertex : POSITION;
                fixed4 color : COLOR;
                float2 uv : TEXCOORD0;
                float2 settingIndex : TEXCOORD2;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct v2f
            {
                fixed4 color : COLOR;
                float2 uv : TEXCOORD0; // uv.z is used for setting index
                float2 settingIndex : TEXCOORD2;
                float4 vertex : SV_POSITION;
                UNITY_VERTEX_OUTPUT_STEREO
            };

            sampler2D _MainTex;
            float4 _MainTex_ST;
            float4 _MainTex_TexelSize;
            fixed4 _Color;
            
            v2f GradientVert (appdata IN)
            {
                v2f OUT;

                UNITY_SETUP_INSTANCE_ID (IN);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(OUT);

                OUT.vertex = UnityObjectToClipPos(IN.vertex);
                #ifdef UNITY_COLORSPACE_GAMMA
                OUT.color = IN.color;
                #else
                OUT.color = fixed4(GammaToLinearSpace(IN.color.rgb), IN.color.a);
                #endif
                OUT.color *= _Color * _RendererColor;
                OUT.uv = TRANSFORM_TEX(IN.uv, _MainTex);
                OUT.settingIndex = IN.settingIndex;
                return OUT;
            }

            fixed4 GradientFrag (v2f i) : SV_Target
            {
                fixed4 gradColor = EvaluateGradient(i.settingIndex.x, i.uv, _MainTex, _MainTex_TexelSize.xy);
                
                #ifndef UNITY_COLORSPACE_GAMMA
                gradColor = fixed4(GammaToLinearSpace(gradColor.rgb), gradColor.a);
                #endif

                fixed4 finalColor = gradColor * i.color;
                finalColor.rgb *= finalColor.a;

                return finalColor;
            }
            ENDCG
        }
    }
}
