Shader "ViveSR/PointCloudGeoShader"
{
	Properties
	{
		_Color ("Base Color", Color) = (1,1,1,1)
		_PointSizeScaler ("PointSize", Range(0.1, 5)) =  0.8
		[Enum(All, 15, None, 0)] _ColorWrite("Color Write", Float) = 15
		_StencilValue ("StencilRefValue", float) = 0
		[Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
	}

	SubShader
	{
		Tags { "RenderType"="Opaque" "Queue" = "Geometry" "LightMode"="ForwardBase" }
		Cull Off
		ColorMask [_ColorWrite]

		Stencil{
			Ref  [_StencilValue]
			Comp [_StencilComp]
		}

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma geometry geom
			#pragma fragment frag
			//#pragma multi_compile_fwdbase
			#pragma multi_compile __ RENDER_AS_BILLBOARD
			#include "UnityCG.cginc"
			#include "AutoLight.cginc"

			struct vInput
			{
				float4 pos : POSITION;				
				float4 color : COLOR;

#ifndef RENDER_AS_BILLBOARD
				float3 normal : NORMAL;
#endif
			};

			struct gInput
			{
				float4 wPos : POSITION;
				float4 tangent  : NORMAL;		// right
				float4 binormal : TEXCOORD1;	// up
				float4 color   : COLOR;
				float radius   : TEXCOORD2;
			};

			struct fInput
			{
				float4 pos : SV_POSITION;
				float4 color  : COLOR;
				//SHADOW_COORDS(1)
			};

			float4 _Color;
			float _PointSizeScaler;		

			gInput vert (vInput vIn)
			{
				gInput vOutput;				

				vOutput.wPos = vIn.pos;
				vOutput.color = vIn.color * _Color;
				vOutput.radius = _PointSizeScaler * 0.01;

#ifdef RENDER_AS_BILLBOARD
				float3 nor = UnityWorldSpaceViewDir(vOutput.wPos);
#else
				float3 nor = normalize(vIn.normal);
#endif

				float3 up = float3(0,1,0);
				float3 right = float3(1,0,0);
				float nDotU = abs(dot(up, nor));
				float nDotR = abs(dot(right, nor));
				if ( nDotU < nDotR )
				{
					right = normalize(cross(up, nor));
					up = normalize(cross(nor, right));
				}
				else
				{
					up = normalize(cross(nor, right));
					right = normalize(cross(up, nor));
				}

				vOutput.tangent = float4(right, 0.0);
				vOutput.binormal = float4(up, 0.0);
				//vOutput.color.xyz = ( nor + 1.0f ) * 0.5f;

				return vOutput;
			}

			[maxvertexcount(24)]
			void geom(point gInput gIn[1], inout TriangleStream<fInput> triStream )
			{
				float4 outPos = gIn[0].wPos;
				float4 right = gIn[0].tangent;
				float4 up = gIn[0].binormal;
				float radius = gIn[0].radius;

				fInput center;
				center.color = gIn[0].color;
				center.pos = UnityObjectToClipPos(outPos);
				//TRANSFER_SHADOW(center)				

				fInput newV;
				newV.color = gIn[0].color;

				const float PI = 3.141592653;
				float angleStep = PI * 0.25;
				for ( uint i = 0; i < 8; ++i )
				{
					triStream.Append(center);
				
					//newV.pos = center.pos;
					//TRANSFER_SHADOW(newV)
					newV.pos = outPos;					
					newV.pos += radius * cos( angleStep * i) * right;
					newV.pos += radius * sin( angleStep * i) * up;
					newV.pos = UnityObjectToClipPos(newV.pos);
					triStream.Append(newV);					
					
					//newV.pos = center.pos;
					//TRANSFER_SHADOW(newV)
					newV.pos = outPos;
					newV.pos += radius * cos( angleStep * (i + 1)) * right;
					newV.pos += radius * sin( angleStep * (i + 1)) * up;
					newV.pos = UnityObjectToClipPos(newV.pos);
					triStream.Append(newV);
				
					triStream.RestartStrip();
				}	
			}
			
			float4 frag (fInput fIn) : SV_Target
			{
				return fIn.color;
				//fixed shadow = SHADOW_ATTENUATION(fIn);
				//float3 outColor = fIn.color.rgb * shadow;

				//return float4(outColor, fIn.color.a);
			}
			ENDCG
		}
	}

	FallBack "Standard"
}
