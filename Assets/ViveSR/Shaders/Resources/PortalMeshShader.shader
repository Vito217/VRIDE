Shader "ViveSR/PortalMeshShader"
{
	Properties
	{
		_StencilValue ("Stencil Write Value", float) = 1
	}
	SubShader
	{
		Tags { "RenderType" = "Opaque" "Queue" = "Background-2" }
		LOD 100

		Pass
		{
		    Cull Off
			ZWrite On
			ColorMask Off
			Stencil{
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

			struct fOutput
			{
				float4 color: COLOR;
			};
			
			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				return o;
			}
			
			fOutput frag (v2f i)
			{				
				fOutput fOut;
				fOut.color = fixed4(1.0, 1.0, 1.0, 1.0);
				return fOut;
			}
			ENDCG
		}
	}
}
