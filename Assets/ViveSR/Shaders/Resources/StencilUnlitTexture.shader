// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "ViveSR/Unlit, Textured, Stencil"
{
	Properties
	{
		_MainTex ("Base Texture", 2D) = "white" {}
		[Enum(Off, 0, On, 1)] _ZWrite("Z Write", int) = 1										// default
		_StencilValue ("StencilRefValue", float) = 0
		[Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable

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
			#pragma multi_compile __ SRWORKS_GAMMA_TO_LINEAR
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"

			sampler2D _MainTex;

			struct vInput
			{
				float4 pos : POSITION;			
				float2 uvCoord : TEXCOORD0;	
			};

			struct fInput
			{
				float4 pos : SV_POSITION;
				float2 uvCoord : TEXCOORD0;	
			};

			fInput vert (vInput vIn)
			{
				fInput vOut;			

				vOut.pos = UnityObjectToClipPos(vIn.pos);				
				vOut.uvCoord = vIn.uvCoord;

				return vOut;
			}
			
			float4 frag (fInput fIn) : SV_Target
			{
				#ifdef SRWORKS_GAMMA_TO_LINEAR
				return float4(GammaToLinearSpace(tex2D(_MainTex, fIn.uvCoord).rgb), 1);
				#else
				return tex2D(_MainTex, fIn.uvCoord);
				#endif
			}
			ENDCG
		}
	}
}
