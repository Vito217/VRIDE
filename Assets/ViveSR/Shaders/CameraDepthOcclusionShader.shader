// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "ViveSR/Camera Depth Occlusion"
{
	Properties
	{
		_MainTex ("Base Texture", 2D) = "white" {}
		_MinDepth ("MinDepth", Range(0, 10)) =  0.2
		_MaxDepth ("MaxDepth", Range(0, 10)) =  2
		[Enum(All, 15, None, 0)] _ColorWrite("Color Write", Float) = 0
	}

	SubShader
	{
		// Make the render queue larger than 1000 and less than 2000.
		Tags { "RenderType"="Transparent" "Queue"="Background+11" }
		ColorMask [_ColorWrite]
		Blend SrcAlpha OneMinusSrcAlpha

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"

			sampler2D _MainTex;
			float _MinDepth;
			float _MaxDepth;

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

			struct fOutput
			{
				float4 color: COLOR;
				float  depth: DEPTH;
			};

			fInput vert (vInput vIn)
			{
				fInput vOut;			

				vOut.pos = UnityObjectToClipPos(vIn.pos);				
				//vOut.uvCoord = vIn.uvCoord;
				vOut.uvCoord.x = vIn.uvCoord.x;
				vOut.uvCoord.y = 1 - vIn.uvCoord.y;

				return vOut;
			}
			
			fOutput frag (fInput fIn)
			{
				fOutput fOut;				
				float viewD = tex2D(_MainTex, fIn.uvCoord).r * 0.01;		// cm to m
				float outAlpha = 1.0f;

				if (viewD > _MaxDepth || viewD < _MinDepth)
				{
					viewD = 1000.0;
					outAlpha = 0.0f;
				}

				// view depth to clip depth
				float clipD = ( (1 / viewD) - _ZBufferParams.w ) / _ZBufferParams.z;
				fOut.color = float4(viewD / _MaxDepth, viewD / _MaxDepth, viewD / _MaxDepth, outAlpha);
				fOut.depth = clipD;

				return fOut;
			}
			ENDCG
		}
	}

	FallBack "Unlit/Texture"
}
