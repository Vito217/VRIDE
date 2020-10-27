Shader "ViveSR/Wireframe"
{
    Properties
    {
        _Thickness("Wire Thickness", RANGE(0, 1300)) = 100
        _Color("Wire Color", Color) = (0.0, 1.0, 0.0, 1.0)
        _Opaque("Opaque", RANGE(0,1)) = 1
        [Enum(Normal, 4, Always, 0)] _ZTest("Z Test Func", Float) = 4
        _StencilValue("StencilRefValue", float) = 0
        [Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
    }

        CGINCLUDE

#include "UnityCG.cginc"
#include "UnityPBSLighting.cginc"
#include "AutoLight.cginc"
#include "../ViveSRCG.cginc"

        uniform float _Thickness;
    uniform float4 _Color;
    uniform float _Opaque;
    sampler2D _PassThroughBeforeWireframeTex;
    DECLARE_CLIP_PLANE_VARIABLE

    struct appdata
    {
        float4 vertex : POSITION;
        float4 vColor : COLOR;
    };

    struct v2g
    {
        float4 cPos : SV_POSITION;
        float4 vColor : COLOR;
        WORLD_POS_FORCLIP(0)
            float4 objPos : TEXCOORD1;
    };

    struct g2f
    {
        float4 pos : SV_POSITION;
        float4 vColor : COLOR;
        float3 screenPos : TEXCOORD0;
        float4 dist : TEXCOORD1;
        WORLD_POS_FORCLIP(2)
            SHADOW_COORDS(3)
    };

    v2g vert(appdata v)
    {
        v2g o;
        o.cPos = UnityObjectToClipPos(v.vertex);
        o.vColor = v.vColor;
        o.objPos = v.vertex;
        COMPUTE_CLIP_WORLD_POS(o, v.vertex)
            return o;
    }

    [maxvertexcount(3)]
    void geom(triangle v2g i[3], inout TriangleStream<g2f> triangleStream)
    {
        float2 p0 = i[0].cPos.xy / i[0].cPos.w;
        float2 p1 = i[1].cPos.xy / i[1].cPos.w;
        float2 p2 = i[2].cPos.xy / i[2].cPos.w;

        float2 edge0 = p2 - p1;
        float2 edge1 = p2 - p0;
        float2 edge2 = p1 - p0;

        // To find the distance to the opposite edge, we take the
        // formula for finding the area of a triangle Area = Base/2 * Height, 
        // and solve for the Height = (Area * 2)/Base.
        // We can get the area of a triangle by taking its cross product
        // divided by 2.  However we can avoid dividing our area/base by 2
        // since our cross product will already be double our area.
        float area = abs(edge1.x * edge2.y - edge1.y * edge2.x);
        float wireThickness = 1500 - _Thickness;

        appdata v;
        g2f o;

        ASSIGN_CLIP_WORLD_POS_GEOSHADER(i[0], o)
            o.pos = i[0].cPos;
        o.vColor = i[0].vColor;
        o.screenPos = o.pos.xyw;
        o.dist.xyz = float3((area / length(edge0)), 0.0, 0.0) * o.pos.w * wireThickness;
        o.dist.w = 1.0 / o.pos.w;
        v.vertex = i[0].objPos;
        TRANSFER_SHADOW(o);
        triangleStream.Append(o);

        ASSIGN_CLIP_WORLD_POS_GEOSHADER(i[1], o)
            o.pos = i[1].cPos;
        o.vColor = i[1].vColor;
        o.screenPos = o.pos.xyw;
        o.dist.xyz = float3(0.0, (area / length(edge1)), 0.0) * o.pos.w * wireThickness;
        o.dist.w = 1.0 / o.pos.w;
        v.vertex = i[1].objPos;
        TRANSFER_SHADOW(o);
        triangleStream.Append(o);

        ASSIGN_CLIP_WORLD_POS_GEOSHADER(i[2], o)
            o.pos = i[2].cPos;
        o.vColor = i[2].vColor;
        o.screenPos = o.pos.xyw;
        o.dist.xyz = float3(0.0, 0.0, (area / length(edge2))) * o.pos.w * wireThickness;
        o.dist.w = 1.0 / o.pos.w;
        v.vertex = i[2].objPos;
        TRANSFER_SHADOW(o);
        triangleStream.Append(o);
    }

    float4 frag(g2f i) : SV_Target
    {
        CLIP_PLANE_TEST(i)

        float2 scrPos = i.screenPos.xy / i.screenPos.z;
        scrPos.x = 0.5 + scrPos.x * 0.5;
        scrPos.y = 0.5 - scrPos.y * 0.5;

        fixed shadow = SHADOW_ATTENUATION(i);

        float minDistanceToEdge = min(i.dist[0], min(i.dist[1], i.dist[2])) * i.dist[3];
        float4 bgColor = tex2D(_PassThroughBeforeWireframeTex, scrPos);

        // Smooth our line out / or not on a line
        float t = (minDistanceToEdge > 0.9) ? 0 : exp2(-2 * minDistanceToEdge * minDistanceToEdge);
        fixed4 finalColor = lerp(bgColor, _Opaque*_Color + (1.0 - _Opaque)*i.vColor, t);
        finalColor.rgb *= shadow;
#ifdef FWDADD
        finalColor.a = 1 - shadow;
#endif
        return finalColor;
    }
        ENDCG

        SubShader
    {
        Tags{ "RenderType" = "Opaque" "Queue" = "Geometry-1" }

            Stencil{
            Ref[_StencilValue]
            Comp[_StencilComp]
        }

            Pass
        {
            ZTest[_ZTest]
            ZWrite On
            ColorMask 0
            Cull Off
        }

            GrabPass{ "_PassThroughBeforeWireframeTex" }

            Pass
        {
            Tags{ "LightMode" = "ForwardBase" }
            Cull Off

            // http://developer.download.nvidia.com/SDK/10/direct3d/Source/SolidWireframe/Doc/SolidWireframe.pdf
            CGPROGRAM
#pragma vertex vert
#pragma geometry geom
#pragma fragment frag
#pragma multi_compile_fwdbase
#pragma multi_compile __  CLIP_PLANE          
            ENDCG
        }

            Pass
        {
            Tags{ "LightMode" = "ForwardAdd" }

            Blend SrcAlpha OneMinusSrcAlpha
            ZTest[_ZTest]
            ZWrite Off
            Cull Off

            CGPROGRAM
#pragma vertex vert
#pragma geometry geom
#pragma fragment frag
#pragma multi_compile_fwdadd_fullshadows
#pragma multi_compile FWDADD
#pragma multi_compile __  CLIP_PLANE          
            ENDCG
        }
    }

    FallBack "Standard"
}
