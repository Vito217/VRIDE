// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "ViveSR/Defish Shader"
{
	Properties
	{
		_MainTex ("Base Texture", 2D) = "white" {}
		_DefishTex ("Defish Texture", 2D) = "white" {}
	}

	SubShader
	{
		Tags { "RenderType"="Opaque" }

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"

			sampler2D _MainTex;
			sampler2D _DefishTex;
			float4 _MainTex_ST;

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
				float2 newUV = tex2D(_DefishTex, fIn.uvCoord).rg;
				newUV = TRANSFORM_TEX(newUV, _MainTex);
				return tex2D(_MainTex, newUV);
			}
			ENDCG
		}
	}
}
