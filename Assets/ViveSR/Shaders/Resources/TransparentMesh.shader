Shader "ViveSR/TransparentMesh"
{
    Properties
    {
        _StencilValue("StencilRefValue", float) = 0
        [Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
    }
        SubShader
    {
        Tags { "RenderType" = "Transparent" "Queue" = "Geometry-1" "LightMode" = "ForwardBase" "ForceNoShadowCasting" = "True" }

        Stencil{
            Ref[_StencilValue]
            Comp[_StencilComp]
        }

        Pass
        {
            Cull Off
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite On

            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            struct appdata
            {
                float4 vertex : POSITION;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
            };

            v2f vert(appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                return o;
            }

            fixed4 frag(v2f i) : SV_Target
            {
                return fixed4(0,0,0,0);
            }
            ENDCG
        }
    }
}
