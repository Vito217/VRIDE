Shader "ViveSR/DepthCleaner"
{
	Properties 
	{
		_StencilValue ("StencilRefValue", float) = 1
		[Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Component", int) = 0	// disable
    }

	SubShader
	{
		Tags { "RenderType" = "Opaque" "Queue" = "Background-1" }
		LOD 100

		Pass
		{
		    Cull Off
			ColorMask 0
			ZTest Always
			ZWrite On

			Stencil
			{
				Ref [_StencilValue]
				Comp [_StencilComp]
			}

			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			
			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
			};

			struct v2f
			{
				float4 vertex : SV_POSITION;
			};
			
			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = float4(v.vertex.xy * 2.0, 0, 1);
				return o;
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				return fixed4(1.0, 1.0, 1.0, 1.0);
			}
			ENDCG
		}
	}
}
