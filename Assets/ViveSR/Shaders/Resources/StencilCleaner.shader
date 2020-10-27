Shader "ViveSR/StencilCleaner"
{
	Properties 
	{
		_StencilValue ("Clear Value", float) = 1
    }

	SubShader
	{
		Tags { "RenderType" = "Opaque" "Queue" = "Background-3" }
		LOD 100

		Pass
		{
		    Cull Off
			ColorMask 0
			ZTest Always
			ZWrite Off

			Stencil
			{
				Ref [_StencilValue]
				Comp Always
				Pass Replace
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
